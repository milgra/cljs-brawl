(ns brawl.world
  (:require
   [cljs.core.async :refer-macros [go]]
   [cljs-http.client :as http]
   [cljs.core.async :refer [<! chan put! take! poll!]]
   [tubax.core :refer [xml->clj]]
   [mpd.phys2 :as phys2]
   [mpd.math2 :as math2]
   [gui.math4 :as math4]
   [brawl.ui :as brawlui]
   [brawl.svg :as svg]
   [brawl.gun :as gun]
   [brawl.webgl :as webgl]
   [brawl.actor :as actor]
   [brawl.names :as names]
   [brawl.metrics :as metrics]
   [brawl.layouts :as layouts]
   [brawl.defaults :as defaults]
   [brawl.particle :as particle]
   [brawl.actorskin :as actorskin]))


(defn init []
  {:inited false
   :loaded false
   :warntime 0
   :warned nil ;; player reached the end without killing all enemy
   :guns {}
   :infos []
   :actors {}
   :endpos [0 0]
   :surfaces []
   :particles []
   :surfacelines []
   :view-rect [0 0 0 0]
   :projection (math4/proj_ortho 50 50 -50 -50 -1.0 1.0)})


(def colors [0xFF0000FF
             0x00FF00FF
             0x0000FFFF
             0xFF00FFFF
             0x00FFFFFFF
             0xFFFF00FF])


(defn create-actors
  "add actors, infos, guns and enpoint to scene based on pivot points in svg"
  [state pivots herometrics currlevel]
  (reduce (fn [{:keys [actors guns infos] :as oldstate}
               {:keys [id path] :as pivot}]         
            (let [pos (nth path 3)
                  tokens (clojure.string/split id #"_")
                  type (first (second tokens))]
              (cond
                (= type "l") (let [count (if (> (count tokens) 3)
                                           (js/parseInt (second (nth tokens 4)))
                                           1)]
                               (assoc oldstate :actors
                                      (reduce (fn [result _]
                                                (let [team (js/parseInt (second (nth tokens 2)))
                                                      level (js/parseInt (second (second tokens)))
                                                      name (if (= level 0) :hero (keyword (names/getname)))
                                                      color (nth colors team)
                                                      metrics (if (= level 0) herometrics (metrics/basemetrics-random))
                                                      actor (actor/init (+ -20 (rand-int 40) (first pos)) (second pos) name color metrics currlevel)]
                                                  (assoc result name actor)))
                                                  actors
                                                  (range 0 count))))
                (= type "g") (let [id (keyword (str "gun" (rand 1000)))
                                   gun (gun/init id pos)]
                               (update oldstate :guns assoc id gun))
                (= type "e") (assoc oldstate :endpos pos)
                (= type "i") (let [index (js/parseInt (second type))]
                               (assoc oldstate :infos (conj infos {:pos pos :index index})))))) state pivots))


(defn create-particles [contacts [dx dy]]
  (reduce (fn [res [x y]]
            (concat
             res
             (repeatedly 10 #(particle/init x y [1.0 1.0 1.0 0.5] (math2/resize-v2 [(+ (- 1.0) (rand 2.0)) (+ (- 1.0) (rand 2.0))] (+ 1.0 (rand 2.0))) :dust))
             (repeatedly 5 #(particle/init x y [1.0 0.0 0.0 0.5] [ (+ dx -2.0 (rand 2.0)) (+ dy -2.0 (rand 2.0))] :blood))))
          []
          contacts))


(defn hit-actors [actors sounds command time]
  (reduce (fn [result [id actor]]
            (let [newactor (actor/hit actor command time)]
              (if (and (> (:health actor) 0 ) (< (:health newactor) 0)) (.play ((keyword (str "death" (rand-int 2))) sounds)))
              (assoc result id newactor)))
          {}
          actors))


(defn execute-attack
  "hittest actors and modify if hit happened"
  [{{:keys [actors particles]} :world sounds :sounds :as state} {id :id :as command} time]
  (let [sender (id actors)]
    (if-not (:dragged-body (:drag sender))
      ;; normal kick/puncj
      (let [main-dir (math2/resize-v2 (math2/sub-v2 (:target command) (:base command)) 4.0)
            contacts (remove nil? (map (fn [[id actor]]

                                         (first (remove nil? (actor/hitpoints actor command)))) actors))
            new-particles (create-particles contacts main-dir)
            new-actors (hit-actors actors sounds command time)]
        (if (get-in sender [:control :shoot]) (.play (:shot sounds))           
            (when-not (empty? contacts)
              (.play ((keyword (str "punch" (rand-int 3))) sounds))
              ;; TODO MOVE THIS!!!
              ;; start music at first punch, should do better but starting music needs user interaction
              (set! (.-loop (:theme sounds)) true)
              (.play (:theme sounds))))
        (-> state
            (assoc-in [:world :particles] (concat particles new-particles))
            (assoc-in [:world :actors] new-actors)))
      ;; throw dragged body
      (let [dragged ((:dragged-body (:drag sender)) actors)
            masses (:masses dragged)
            newmasses (reduce (fn [res [id mass]] ; reset mass directions for next rag
                                      (assoc res id (assoc mass :d [(+ (* 6.0 (:facing sender)) (* (:speed sender) 0.5)) -5])))
                                    masses
                                    masses)
            newdragged (-> dragged
                           (assoc :masses newmasses)
                           (assoc-in [:drag :injure-when-dropped] true)
                           (assoc-in [:drag :is-dragged] false))
            newsender (assoc sender :dragged-body nil)]
        (-> state
            (assoc-in [:world :actors id] newsender)
            (assoc-in [:world :actors (:id dragged)] newdragged))))))
    

(defn calc-view-rect
  "calculates the visible rectangle of the world"
  [{{:keys [actors warned warntime] :as world} :world :as state} time]  
  (let [actor (if (or (> time warntime) (= nil warned)) (:hero actors) (warned actors))
        [fax fay] (:p (get-in actor [:bases :base_l]))
        [fbx fby] (:p (get-in actor [:bases :base_r]))
        [tx ty] [ (+ fax (/ (- fbx fax ) 2)) (+ fay (/ (- fby fay) 2) -50.0)  ]
        r (/ (.-innerWidth js/window) (.-innerHeight js/window) )
        h 300.0
        w (* h r)
        [l r b t :as new-rect] [(- tx w) (+ tx w) (+ ty h) (- ty h)]
        new-proj (math4/proj_ortho (+ l 50) (- r 50) (- b 50) (+ t 50) -1.0 1.0)]
    (-> state
        (assoc-in [:world :view-rect] new-rect)
        (assoc-in [:world :projection] new-proj))))


(defn check-ended [{:keys [commands-world] {:keys [actors finished warned warntime endpos] :as world} :world :as state} time]
  (let [[ex ey] endpos
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


(defn update-particles [{{:keys [particles view-rect]} :world :as state}]
  (let [new-particles (map (fn [particle] (particle/upd particle view-rect)) particles)]
    (assoc-in state [:world :particles] new-particles)))


(defn update-guns [{{:keys [guns actors] :as world} :world :as state}]
  (let [new-guns (reduce (fn [result [id {{:keys [dragged-gun]} :drag :as actor}]]
                           (if-not dragged-gun
                             result
                             (assoc result dragged-gun (actor/update-gun (dragged-gun guns) actor)))) guns actors)]
    (assoc-in state [:world :guns] new-guns)))


(defn update-dragged [{{:keys [guns actors] :as world} :world :as state} time]
  (let [new-actors (reduce (fn [result [id {{:keys [dragged-body]} :drag :as actor}]]
                           (if-not dragged-body
                             result
                             (assoc result dragged-body (actor/update-dragged (dragged-body result) actor time)))) actors actors)]
    (assoc-in state [:world :actors] new-actors)))


(defn extract-actor-commands [{:keys [commands-world]
                               {:keys [actors] :as world} :world :as state}]
  (let [new-commands (reduce (fn [result [id actor]]
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


(defn update-actors [{:keys [controls]
                      {:keys [actors surfaces guns] :as world} :world :as state}
                     time
                     delta]
  (let [new-actors (reduce (fn [result [id actor]]
                             (let [newactor (cond
                                              (not= :hero id) (actor/update-actor actor nil surfaces result guns time delta)
                                              :else (actor/update-actor actor controls surfaces result guns time delta))]
                               (assoc result id newactor)))
                           {}
                           actors)]
    (assoc-in state [:world :actors] new-actors)))


(defn update-world
  "updates phyisics and actors"
  [{{loaded :loaded} :world :as state} msg time delta]
  (if-not loaded
    state
    (-> state
        (update-actors time delta)
        (extract-actor-commands)
        (update-guns)
        (update-dragged time)
        (check-ended time)
        (calc-view-rect time)
        (update-particles))))


(defn init-seeds [{{:keys [view-rect] :as world} :world :as state}]
  (let [[l r b t] (:view-rect world)
        seeds (map #(particle/init (+ l (rand (- r l))) t [1.0 1.0 1.0 0.5] [(+ 0.1 (rand 0.6)) (+ 0.05 (rand 0.3))] :seed) (range 0 20))]
    (assoc-in state [:world :particles] seeds)))
 

(defn reset-world [{:keys [level world world-drawer metrics] :as state} msg time]
  (if (and msg (= (:id msg) "level"))
    (let [svglevel (:shapes msg)
          points (map :path (filter #(and (= (% :id) "Surfaces") (not (contains? % :color))) svglevel ))
          pivots (filter #(if (:id %) (clojure.string/includes? (:id %) "Pivot") false) svglevel)
          surfaces (phys2/surfaces-from-pointlist points)
          lines (clj->js (reduce (fn [result {[tx ty] :t [bx by] :b}] (concat result [tx ty 1.0 1.0 1.0 1.0 (+ tx bx) (+ ty by) 1.0 1.0 1.0 1.0])) [] surfaces))
          newdrawer (webgl/loadshapes world-drawer (filter #(if (:id %) (not (clojure.string/includes? (:id %) "Pivot")) true) (:shapes msg)))
          newworld (-> {:actors {}
                        :guns {}
                        :infos []
                        :inited true
                        :loaded true
                        :warned nil
                        :warntime 0
                        :finished false
                        :particles []
                        :surfaces surfaces
                        :surfacelines lines}
                       (create-actors pivots metrics (:level state)))]
      (cond-> state
        true (assoc :world-drawer newdrawer)
        true (assoc :world newworld)
        true (assoc :commands-world [])
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
          shapes (svg/psvg xmlstr "")]
      (put! channel {:id "level" :shapes shapes}))))


(defn load-first-level [{:keys [level sounds] :as state}]
  (load-level! state 0)
  (-> state
      (assoc-in [:world :loaded] false)
      (assoc :level 0)
      (defaults/save-defaults!)))


(defn load-next-level [{:keys [level sounds] :as state}]
  (let [next-level (min (inc level) 6)]
    (load-level! state next-level)
    (-> state
        (assoc-in [:world :loaded] false)
        (assoc :level next-level)
        (defaults/save-defaults!))))


(defn load-same-level [{:keys [level sounds] :as state}]
  (load-level! state level)
  (-> state
      (assoc-in [:world :loaded] false)))


(defn pickup-object [{{:keys [actors guns]} :world {:keys [dragged-gun dragged-body]} :drag :as oldstate} {:keys [id text]}]
  (let [actor (id actors)
        {{hip :hip} :masses color :color :as actor} actor
        ;; look for gun
        nearby-gun (first (remove nil? (map (fn [[_ {:keys [id p d]}]]
                                              (if-not (< (math2/length-v2 (math2/sub-v2 p (:p hip))) 80.0) nil id)) guns)))

        nearby-actor (first (remove nil? (map (fn [[_ {{ehip :hip} :masses ecolor :color ehealth :health eid :id {edragged :is-dragged} :drag}]]
                                                (let [dist (math2/length-v2 (math2/sub-v2 (:p ehip) (:p hip)))]
                                                  (if (and (< ehealth 0) (not= id eid) (not edragged) (< dist 150.0)) eid nil))) actors)))

        dragged-actor (if nearby-actor
                        (-> (nearby-actor actors)
                            (assoc-in [:drag :is-dragged] true)
                            (assoc :next-mode :rag)))
        new-actor (cond-> actor
                    nearby-gun
                    (assoc-in [:drag :dragged-gun] nearby-gun)
                    nearby-gun
                    (assoc :bullets 6)
                    nearby-actor
                    (assoc-in [:drag :dragged-body] nearby-actor))]
    (cond-> oldstate
        true (assoc-in [:world :actors id] new-actor)
        nearby-actor (assoc-in [:world :actors nearby-actor] dragged-actor))))


(defn execute-commands
  [{:keys [level commands-world ui-drawer] :as state} time]
  (reduce
   (fn [oldstate {text :text :as command}]
     (let [hero (get-in oldstate [:worlds :actors :hero])
           path-metrics [:world :actors :hero :metrics]]
       (cond

         (= text "pickup")
         (pickup-object oldstate command)

         (= text "attack")
         (execute-attack oldstate command time)

         (= text "new-game")
         (load-first-level oldstate)

         (= text "next-level")
         (if (= level 6)
           ;; show congrats screen
           (brawlui/load-ui oldstate layouts/finished)
           ;; load next level
           (-> oldstate
               (brawlui/load-ui layouts/info)
               (update :commands-world conj {:text "load-level"})))

         (= text "load-level") (load-next-level oldstate)

         (= text "restart-level") (load-same-level oldstate)

         (= text "show-wasted") (brawlui/load-ui oldstate layouts/wasted)

         :else oldstate)))
   (assoc state :commands-world [])
   commands-world))
