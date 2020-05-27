(ns brawl.world
  (:require
   [cljs.core.async :refer-macros [go]]
   [cljs-http.client :as http]
   [cljs.core.async :refer [<! chan put! take! poll!]]
   [tubax.core :refer [xml->clj]]
   [mpd.phys2 :as phys2]
   [mpd.math2 :as math2]
   [gui.math4 :as math4]
   [brawl.names :as names]
   [brawl.particle :as particle]
   [brawl.webgl :as webgl]
   [brawl.svg :as svg]
   [brawl.ui :as brawlui]
   [brawl.layouts :as layouts]
   [brawl.actor :as actor]
   [brawl.actorskin :as actorskin]))


(defn init []
  {:inited false
   :loaded false
   :guns []
   :infos []
   :actors {}
   :endpos [0 0]
   :surfaces []
   :particles []
   :surfacelines []
   :view-rect [0 0 0 0]
   :projection (math4/proj_ortho 50 50 -50 -50 -1.0 1.0)})


(def colors [0xFF0000FF 0x00FF00FF 0x0000FFFF 0xFF00FFFF 0x00FFFFFFF 0xFFFF00FF])

(defn create-actors
  "add actors, infos, guns and enpoint to scene based on pivot points in svg"
  [state pivots]
  (reduce (fn [{:keys [actors guns infos] :as oldstate} {id :id path :path}]         
            (let [pos (nth path 3)
                  toks (clojure.string/split id #"_")
                  type (first (second toks))]
              (cond
                (= type "l") (let [team (js/parseInt (second (nth toks 2)))
                                   level (js/parseInt (second (second toks)))
                                   name (if (= level 0) :hero (keyword (names/getname)))
                                   color (nth colors team)]
                               (update oldstate :actors assoc name (actor/init (first pos) (second pos) name color)))
                (= type "g") (assoc oldstate :guns (conj guns {:pos pos}))
                (= type "e") (assoc oldstate :endpos pos)
                (= type "i") (assoc oldstate :infos (conj infos {:pos pos :index (js/parseInt (second type))})))     
              )) state pivots))


(defn execute-attack [{{actors :actors particles :particles} :world sounds :sounds :as state} command]
  ;; hittest other actors
  (let [[dx dy] (math2/resize-v2 (math2/sub-v2 (:target command) (:base command)) 4.0)
        contacts (remove nil? (map (fn [[id actor]] (actor/hitpoint actor command)) actors))
        newparticles (reduce (fn [res [x y]]
                               (concat res
                                       (repeatedly 10 #(particle/init x y [1.0 1.0 1.0 0.5] (math2/resize-v2 [(+ (- 1.0) (rand 2.0)) (+ (- 1.0) (rand 2.0))] (+ 1.0 (rand 2.0))) :dust))
                                       (repeatedly 5 #(particle/init x y [1.0 0.0 0.0 0.5]  [ (+ dx -2.0 (rand 2.0)) (+ dy -2.0 (rand 2.0))] :blood))))
                             [] contacts)
        newactors (reduce (fn [result [id actor]]
                            (let [newactor (actor/hit actor command)]
                              (if (and (> (:health actor) 0 ) (< (:health newactor) 0)) (.play ((keyword (str "death" (rand-int 2))) sounds)))
                              (assoc result id newactor)))
                          {}
                          actors)]
    (when-not (empty? contacts)
      (.play ((keyword (str "punch" (rand-int 3))) sounds))
      (set! (.-loop (:theme sounds)) true)
      (.play (:theme sounds)))
  
    (-> state
        (assoc-in [:world :particles] (concat particles newparticles))
        (assoc-in [:world :actors] newactors ))))


(defn calc-view-rect
  "calculates the visible rectangle of the world"
  [actor]
  (let [[fax fay] (:p (get-in actor [:bases :base_l]))
        [fbx fby] (:p (get-in actor [:bases :base_r]))
        [tx ty] [ (+ fax (/ (- fbx fax ) 2)) (+ fay (/ (- fby fay) 2))  ]
        r (/ (.-innerWidth js/window) (.-innerHeight js/window) )
        h 350.0
        w (* h r)]
    [(- tx w) (+ tx w) (+ ty h) (- ty h)]))


(defn update-world
  "updates phyisics and actors"
  [{{:keys [actors surfaces particles inited loaded endpos view-rect finished] :as world} :world keycodes :keycodes level :level commands-world :commands-world sounds :sounds controls :controls :as state} msg]
  (if loaded
    (let [newactors (reduce (fn [result [id actor]]
                              (let [newactor (cond
                                               (not= :hero id) (actor/update-actor actor nil surfaces actors 1.0)
                                               :else (actor/update-actor actor controls surfaces actors 1.0))]
                                (assoc result id newactor)))
                            {}
                            actors)

          ;; extract commands
          newcommands (reduce (fn [result [id actor]]
                                (if (empty? (:commands actor))
                                  result
                                  (conj result (:commands actor))))
                              commands-world
                              newactors)

          ;; remove commands if needed
          newnewactors (if (empty? newcommands)
                         newactors
                         (reduce (fn [result [id actor]]
                                   (if (empty? (:commands actor))
                                     result
                                     (assoc result id (-> actor (assoc :commands []) (assoc :action-sent true)))))
                                 newactors
                                 newactors))
          ;; finished sign?
          ended (let [[ex ey] endpos
                      [bx by] (get-in newactors [:hero :bases :base_l :p])
                      dx (- bx ex)
                      dy (- by ey)]
                  (if (and (< (Math/abs dx) 50.0) (< (Math/abs dy) 50)) true false))
          
          newnewcommands (if (and (not finished) ended) (conj newcommands {:text "next-level"}) newcommands)

          ;; new view rectangle based on hero position
          [l r b t :as new-rect] (calc-view-rect (:hero newnewactors))

          new-projection (math4/proj_ortho (+ l 50) (- r 50) (- b 50) (+ t 50) -1.0 1.0)

          newparticles (map (fn [particle] (particle/upd particle new-rect)) particles)

          newworld (assoc world :actors newnewactors :particles newparticles :finished ended :view-rect new-rect :projection new-projection)]

      (-> state
          (assoc :commands-world newnewcommands)
          (assoc :world newworld)))
    state))


(defn reset-world [{:keys [level view-rect world-drawer] :as state} msg]
  (if (and msg (= (:id msg) "level"))
    (let [svglevel (:shapes msg)
          [l r b t] view-rect
          points (map :path (filter #(and (= (% :id) "Surfaces") (not (contains? % :color))) svglevel ))
          pivots (filter #(if (:id %) (clojure.string/includes? (:id %) "Pivot") false) svglevel)
          surfaces (phys2/surfaces-from-pointlist points)
          lines (clj->js (reduce (fn [result {[tx ty] :t [bx by] :b}] (concat result [tx ty 1.0 1.0 1.0 1.0 (+ tx bx) (+ ty by) 1.0 1.0 1.0 1.0])) [] surfaces))
          seeds (map #(particle/init 0.0 0.0 [1.0 1.0 1.0 0.5] [(+ 0.1 (rand 0.6)) (+ 0.05 (rand 0.3))]  :seed) (range 0 20))
          newdrawer (webgl/loadshapes world-drawer (filter #(if (:id %) (not (clojure.string/includes? (:id %) "Pivot")) true) (:shapes msg)))
          newworld (-> {:actors {}
                        :guns []
                        :infos []
                        :inited true
                        :loaded true
                        :finished false
                        :particles seeds
                        :surfaces surfaces
                        :surfacelines lines}
                       (create-actors pivots))]

      (cond-> state
        true (assoc :world-drawer newdrawer)
        true (assoc :world newworld)
        true (brawlui/update-gen-sliders)
        true (assoc :commands-world [])
        (= level 0) (brawlui/load-ui layouts/generator)
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
      (assoc :level 0)))


(defn load-next-level [{:keys [level sounds] :as state}]
  (let [next-level (min (inc level) 6)]
    (load-level! state next-level)
    (-> state
        (assoc-in [:world :loaded] false)
        (assoc :level next-level))))


(defn execute-commands
  [{:keys [level] commands :commands-world ui-drawer :ui-drawer :as state}]
  (reduce
   (fn [oldstate {text :text :as command}]
     (let [hero (get-in oldstate [:worlds :actors :hero])
           path-metrics [:world :actors :hero :metrics]]
       (cond
         (= text "attack")
         (execute-attack oldstate command)

         (= text "new game")
         (load-first-level oldstate)

         (= text "next-level")
         (if (= level 6)
           ;; show congrats screen
           (brawlui/load-ui oldstate layouts/info)
           ;; load next level
           (-> oldstate
               (brawlui/load-ui layouts/info)
               (update :commands conj {:text "load-level"})))

         (= text "load-level") (load-next-level oldstate)

         :else oldstate)))
   (assoc state :commands-world [])
   commands))
