(ns brawl.world
  (:require
    [brawl.actor :as actor]
    [brawl.actorskin :as actorskin]
    [brawl.defaults :as defaults]
    [brawl.gun :as gun]
    [brawl.layouts :as layouts]
    [brawl.metrics :as metrics]
    [brawl.names :as names]
    [brawl.particle :as particle]
    [brawl.svg :as svg]
    [brawl.ui :as brawlui]
    [brawl.webgl :as webgl]
    [cljs-http.client :as http]
    [cljs.core.async :refer-macros [go]]
    [cljs.core.async :refer [<! chan put! take! poll!]]
    [gui.math4 :as math4]
    [mpd.math2 :as math2]
    [mpd.phys2 :as phys2]
    [tubax.core :refer [xml->clj]]))


(defn init
  "creates new world"
  []
  {:inited false
   :loaded false
   :warntime 0
   :warned nil ;; player reached the end without killing all enemy
   :guns {}
   :infos []
   :actors {}
   :endpos [0 0]
   :viewpos [0 0]
   :surfaces []
   :particles []
   :surfacelines []
   :view-rect [0 0 0 0]
   :projection (math4/proj_ortho 50 50 -50 -50 -1.0 1.0)})


(def colors
  [0xFF0000FF
   0x00FF00FF
   0x0000FFFF
   0xFF00FFFF
   0x00FFFFFFF
   0xFFFF00FF])


(defn create-actor
  [tokens herometrics pos currlevel]
  (let [team (js/parseInt (second (nth tokens 2)))
        level (js/parseInt (second (second tokens)))
        name (if (= level 0) :hero (keyword (names/getname)))
        color (nth colors team)
        metrics (if (= level 0) herometrics (metrics/basemetrics-random))]
    (actor/init (+ -20 (rand-int 40) (first pos)) (second pos) name color metrics currlevel)))


(defn create-actors
  [state tokens herometrics pos currlevel]
  (let [{:keys [actors]} state
        count 1 ;;(if (> (count tokens) 3) (js/parseInt (second (nth tokens 4))) 1)
        actors-new (reduce (fn [result _]
                             (let [actor (create-actor tokens herometrics pos currlevel)]
                               (assoc result (:id actor) actor)))
                     actors
                     (range 0 count))]
    (assoc state :actors actors-new)))


(defn create-infos
  [state pos type]
  (let [{:keys [infos]} state
        index (js/parseInt (second type))]
    (assoc state :infos (conj infos {:pos pos :index index}))))


(defn create-guns
  [state pos]
  (let [id (keyword (str "gun" (rand 1000)))
        gun (gun/init id pos)]
    (update state :guns assoc id gun)))


(defn create-objects
  "add actors, infos, guns and enpoint to scene based on pivot points in svg"
  [state pivots herometrics currlevel]
  (reduce (fn [{:keys [actors guns infos] :as oldstate}
               {:keys [id path] :as pivot}]
            (let [pos (nth path 3)
                  tokens (clojure.string/split id #"_")
                  type (first (second tokens))]
              (case type
                "l" (create-actors oldstate tokens herometrics pos currlevel)
                "g" (create-guns oldstate pos)
                "e" (assoc oldstate :endpos pos)
                "i" (create-infos oldstate pos type)))) state pivots))


(defn create-particles
  [contacts [dx dy]]
  (reduce (fn [res [x y]]
            (concat
              res
              (repeatedly 5 #(particle/init x y [1.0 0.0 0.0 0.5] [(+ dx -2.0 (rand 2.0)) (+ dy -2.0 (rand 2.0))] :blood))))
    []
    contacts))


(defn normal-attack
  "punch/kick/shoot"
  [state command time]
  (let [{:keys [actors particles]} (:world state)
        {:keys [id]} command
        sender (id actors)
        main-dir (math2/resize-v2 (math2/sub-v2 (:target command) (:base command)) 4.0)
        [id isps :as attacked] (first (reduce (fn [res [id actor]]
                                                (let [isps (actor/hitpoints actor command)]
                                                  (if (empty? (remove nil? isps)) res (conj res [id isps])))) [] actors))]
    (if-not attacked
      state
      (let [new-particles (create-particles [(first (remove nil? isps))] main-dir)
            new-actor (actor/hit (id actors) command time)]
        (-> state
          (assoc-in [:world :particles] (concat particles new-particles))
          (assoc-in [:world :actors id] new-actor))))))


(defn throw-body
  "throw dragged body"
  [state command time]
  (let [{:keys [actors particles]} (:world state)
        {:keys [sounds]} state
        {:keys [id]} command
        sender (id actors)
        dragged ((get-in sender [:drag :body]) actors)
        masses (:masses dragged)
        newmasses (reduce (fn [res [id mass]]
; reset mass directions for next rag
                            (assoc res id (assoc mass :d [(+ (* 6.0 (:facing sender)) (* (:speed sender) 0.5)) -2])))
                    masses
                    masses)
        newdragged (-> dragged
                     (assoc :masses newmasses)
                     (assoc-in [:drag :injure?] true)
                     (assoc-in [:drag :dragged?] false)
                     (assoc-in [:attack :timeout] (+ time 500)))
        newsender (assoc-in sender [:drag :body] nil)]
    (-> state
      (assoc-in [:world :actors id] newsender)
      (assoc-in [:world :actors (:id dragged)] newdragged))))


(defn execute-attack
  "hittest actors and modify if hit happened"
  [state command time]
  (let [{:keys [actors]} (:world state)
        {:keys [id]} command
        sender (id actors)]
    (if-not (:body (:drag sender))
      (normal-attack state command time)
      (throw-body state command time))))


(defn calc-view-rect
  "calculates the visible rectangle of the world"
  [state time]
  (let [{:keys [actors warned warntime viewpos] :as world} (:world state)
        actor (if (or (> time warntime) (= nil warned)) (:hero actors) (warned actors))
        [vx vy] viewpos
        [fax fay] (:p (get-in actor [:bases :base_l]))
        [fbx fby] (:p (get-in actor [:bases :base_r]))
        [hx hy] [(+ fax (/ (- fbx fax) 2)) (+ fay (/ (- fby fay) 2))]
        [tx ty] [(+ vx (/ (- hx vx) 6)) (+ vy (/ (- hy vy) 6) -10)]
        r (/ (.-innerWidth js/window) (.-innerHeight js/window))
        h 300.0
        w (* h r)
        [l r b t :as new-rect] [(- tx w) (+ tx w) (+ ty h) (- ty h)]
        new-proj (math4/proj_ortho (+ l 50) (- r 50) (- b 50) (+ t 50) -1.0 1.0)]
    (-> state
      (assoc-in [:world :viewpos] [tx ty])
      (assoc-in [:world :view-rect] new-rect)
      (assoc-in [:world :projection] new-proj))))


(defn check-ended
  "check if position is close to ending position and all enemies are dead"
  [state time]
  (let [{:keys [commands-world]} state
        {:keys [actors finished warned warntime endpos]} (:world state)
        [ex ey] endpos
        [bx by] (get-in actors [:hero :bases :base_l :p])
        dx (- bx ex)
        dy (- by ey)
        ended? (if (and (< (Math/abs dx) 50.0) (< (Math/abs dy) 50)) true false)]
    (if-not ended?
      (assoc-in state [:world :warned] nil)
      (let [all-dead (filter (fn [{:keys [health color]}] (and (> health 0) (not= color 0xFF0000FF))) (vals actors))
            new-commands (if (and (not finished) ended?) (conj commands-world {:text "next-level"}) commands-world)]
        (if (empty? all-dead)
          (-> state
            (assoc :commands-world new-commands)
            (assoc-in [:world :finished] ended?))
          (if-not warned
            (-> state
              (assoc-in [:world :warntime] (+ time 1000))
              (assoc-in [:world :warned] (:id (first all-dead))))
            state))))))


(defn update-particles
  "step with particles"
  [state]
  (let [{:keys [particles view-rect]} (:world state)
        new-particles (map (fn [particle] (particle/update-particle particle view-rect)) particles)]
    (assoc-in state [:world :particles] new-particles)))


(defn update-guns
  "step with guns"
  [state]
  (let [{:keys [guns actors]} (:world state)
        new-guns (reduce (fn [result [id {{:keys [gun]} :drag :as actor}]]
                           (if-not gun
                             result
                             (assoc result gun (actor/update-gun (gun guns) actor)))) guns actors)]
    (assoc-in state [:world :guns] new-guns)))


(defn update-dragged
  "step with dragged"
  [state time]
  (let [{:keys [guns actors]} (:world state)
        new-actors (reduce (fn [result [id {{:keys [body]} :drag :as actor}]]
                             (if-not body
                               result
                               (assoc result body (actor/update-dragged (body result) actor time)))) actors actors)]
    (assoc-in state [:world :actors] new-actors)))


(defn extract-actor-commands
  "extract commands from actors"
  [state]
  (let [{:keys [commands-world]} state
        {:keys [actors]} (:world state)

        new-commands (reduce (fn [result [id actor]]
                               (if (empty? (:commands actor))
                                 result
                                 (into result (:commands actor))))
                       commands-world
                       actors)
        new-actors (if (empty? new-commands)
                     actors
                     (reduce (fn [result [id actor]]
                               (if (empty? (:commands actor))
                                 result
                                 (assoc result id (-> actor (assoc :commands [])))))
                       actors
                       actors))]
    (-> state
      (assoc :commands-world new-commands)
      (assoc-in [:world :actors] new-actors))))


(defn update-actors
  "step with actors"
  [state time delta]
  (let [{:keys [controls]} state
        {:keys [actors surfaces guns]} (:world state)
        new-actors (reduce (fn [result [id actor]]
                             (let [newactor (cond
                                              (not= :hero id) (actor/update-actor actor nil surfaces result guns time delta)
                                              :else (actor/update-actor actor controls surfaces result guns time delta))]
                               (assoc result id newactor)))
                     actors
                     actors)]
    (assoc-in state [:world :actors] new-actors)))


(defn update-world
  "updates phyisics and actors"
  [state msg time delta]
  (let [{loaded :loaded} (:world state)]
    (if-not loaded
      state
      (-> state
        (update-actors time delta)
        (extract-actor-commands)
        (update-guns)
        (update-dragged time)
        (check-ended time)
        (calc-view-rect time)
        (update-particles)))))


(defn init-seeds
  [state]
  (let [{:keys [world]} state
        {:keys [view-rect]} world
        [l r b t] (:view-rect world)
        seeds (map #(particle/init (+ l (rand (- r l))) t [1.0 1.0 1.0 0.5] [(+ 0.1 (rand 0.6)) (+ 0.05 (rand 0.3))] :seed) (range 0 20))]
    (assoc-in state [:world :particles] seeds)))


(defn init-viewpos
  [state]
  (let [hero (get-in state [:world :actors :hero])
        hip (get-in hero [:masses :hip :p])]
    (assoc-in state [:world :viewpos] hip)))


(defn reset-world
  [state msg time]
  (if (and msg (= (:id msg) "level"))
    (let [{:keys [level world world-drawer metrics]} state
          svglevel (:shapes msg)
          points (map :path (filter #(and (= (% :id) "Surfaces") (not (contains? % :color))) svglevel))
          pivots (filter #(if (:id %) (clojure.string/includes? (:id %) "Pivot") false) svglevel)
          surfaces (phys2/surfaces-from-pointlist points)
          lines (clj->js (reduce (fn [result {[tx ty] :t [bx by] :b}] (concat result [tx ty 1.0 1.0 1.0 1.0 (+ tx bx) (+ ty by) 1.0 1.0 1.0 1.0])) [] surfaces))
          newdrawer (webgl/load-shapes world-drawer (filter #(if (:id %) (not (clojure.string/includes? (:id %) "Pivot")) true) (:shapes msg)))
          newworld (-> (init)
                     (assoc :inited true)
                     (assoc :loaded true)
                     (assoc :surfaces surfaces)
                     (assoc :surfacelines lines)
                     (create-objects pivots metrics (:level state)))]
      (cond-> state
        true (assoc :world-drawer newdrawer)
        true (assoc :world newworld)
        true (assoc :commands-world [])
        true (init-viewpos)
        true (calc-view-rect time)
        true (init-seeds)
        (= level 0) (brawlui/load-ui layouts/generator)
        (= level 0) (brawlui/update-gen-sliders)
        (> level 0) (brawlui/load-ui layouts/hud)))
    state))


(defn load-level!
  "load level svg's"
  [state level]
  (go
    (let [channel (:msgch state)
          response (<! (http/get (str "levels/level" level ".svg")
                         {:with-credentials? false}))
          xmlstr (xml->clj (:body response) {:strict false})
          shapes (svg/parse-svg xmlstr "")]
      (put! channel {:id "level" :shapes shapes}))))


(defn load-first-level
  [state]
  (let [{:keys [level sounds]} state]
    (load-level! state 0)
    (-> state
      (assoc-in [:world :loaded] false)
      (assoc :level 0)
      (defaults/save-defaults!))))


(defn load-next-level
  [state]
  (let [{:keys [level sounds]} state
        next-level (min (inc level) 6)]
    (load-level! state next-level)
    (-> state
      (assoc-in [:world :loaded] false)
      (assoc :level next-level)
      (defaults/save-defaults!))))


(defn load-same-level
  [state]
  (let [{:keys [level sounds]} state]
    (load-level! state level)
    (-> state
      (assoc-in [:world :loaded] false))))


(defn pickup-object
  [state command]
  (let [{:keys [actors guns]} (:world state)
        {:keys [id text]} command
        actor (id actors)
        {:keys [gun body]} (:drag actor)
        {{hip :hip} :masses color :color :as actor} actor
        ;; look for gun
        nearby-gun (->> (vals guns)
                     (filter #(not (:dragged? %)))
                     (filter (fn [{:keys [id p d]}] (< (math2/length-v2 (math2/sub-v2 p (:p hip))) 80.0)))
                     (first))

        nearby-body (->> (vals actors)
                      (filter #(= (:mode %) :idle))
                      (filter #(< (:health %) 0))
                      (filter #(not (get-in % [:drag :dragged?])))
                      (filter #(not= (:id %) id))
                      (filter (fn [{{ehip :hip} :masses}] (< (math2/length-v2 (math2/sub-v2 (:p ehip) (:p hip))) 150.0)))
                      (first))

        dragged-body (if (and (not body) nearby-body)
                       (-> nearby-body
                         (assoc-in [:drag :dragged?] true)
                         (assoc :next-mode :rag)))

        dragged-gun (if (and (not gun) nearby-gun (= id :hero))
                      (assoc nearby-gun :dragged? true))

        new-actor (cond-> actor
                    dragged-gun
                    (assoc-in [:drag :gun] (:id nearby-gun))
                    dragged-gun
                    (assoc-in [:attack :bullets] 6)
                    dragged-body
                    (assoc-in [:drag :body] (:id nearby-body)))]
    (cond-> state
      true (assoc-in [:world :actors id] new-actor)
      dragged-gun (assoc-in [:world :guns (:id dragged-gun)] dragged-gun)
      dragged-body (assoc-in [:world :actors (:id dragged-body)] dragged-body))))


(defn drop-object
  [state command]
  (let [{:keys [actors guns]} (:world state)
        {:keys [gun body]} (:drag state)
        {:keys [id text]} command
        actor (id actors)
        {:keys [gun body injure? dragged?]} actor
        ;; look for gun
        dragged-gun (if gun
                      (-> (gun guns)
                        (assoc :dragged? false))
                      nil)

        dragged-actor (if body
                        (-> (body actors)
                          (assoc-in [:drag :dragged?] false)
                          (assoc-in [:drag :injure?] false))
                        nil)

        new-actor (-> actor
                    (assoc-in [:drag :gun] nil)
                    (assoc-in [:drag :body] nil))]
    (cond-> state
      true (assoc-in [:world :actors id] new-actor)
      dragged-gun (assoc-in [:world :guns (:id dragged-gun)] dragged-gun)
      dragged-actor (assoc-in [:world :actors (:id dragged-actor)] dragged-actor))))


(defn execute-command
  [state command time]
  (let [{:keys [level sounds]} state
        hero (get-in state [:worlds :actors :hero])
        path-metrics [:world :actors :hero :metrics]]
    (case (:text command)
      "pickup"        (pickup-object state command)
      "drop"          (drop-object state command)
      "attack"        (execute-attack state command time)
      "new-game"      (load-first-level state)
      "next-level"    (if (= level 6)
                        ;; show congrats screen
                        (brawlui/load-ui state layouts/finished)
                        ;; load next level
                        (-> state
                          (brawlui/load-ui layouts/info)
                          (update :commands-world conj {:text "load-level"})))
      "load-level"    (load-next-level state)
      "restart-level" (load-same-level state)
      "show-wasted"   (brawlui/load-ui state layouts/wasted)
      "play-death"    (do (.play ((keyword (str "death" (rand-int 2))) sounds)) state)
      "play-hit"      (do (.play ((keyword (str "punch" (rand-int 3))) sounds)) state)
      "play-shot"     (do (.play (:shot sounds)) state)
      state)))


(defn execute-commands
  [state time]
  (let [{:keys [level commands-world ui-drawer]} state]
    (reduce
      (fn [oldstate command]
        (execute-command oldstate command time))
      (assoc state :commands-world [])
      commands-world)))
