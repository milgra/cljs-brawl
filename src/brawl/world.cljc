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
  [state pivots herometrics]
  (reduce (fn [{:keys [actors guns infos] :as oldstate}
               {:keys [id path] :as pivot}]         
            (let [pos (nth path 3)
                  tokens (clojure.string/split id #"_")
                  type (first (second tokens))]
              (cond
                (= type "l") (let [team (js/parseInt (second (nth tokens 2)))
                                   level (js/parseInt (second (second tokens)))
                                   name (if (= level 0) :hero (keyword (names/getname)))
                                   color (nth colors team)
                                   metrics (if (= level 0) herometrics (metrics/basemetrics-random))
                                   actor (actor/init (first pos) (second pos) name color metrics)]
                               (update oldstate :actors assoc name actor))
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


(defn hit-actors [actors sounds command]
  (reduce (fn [result [id actor]]
            (let [newactor (actor/hit actor command)]
              (if (and (> (:health actor) 0 ) (< (:health newactor) 0)) (.play ((keyword (str "death" (rand-int 2))) sounds)))
              (assoc result id newactor)))
          {}
          actors))


(defn execute-attack
  "hittest actors and modify if hit happened"
  [{{:keys [actors particles]} :world sounds :sounds :as state} {id :id :as command}]
  (let [sender (id actors)]
    (if-not (:dragged-body sender)
      ;; normal kick/puncj
      (let [main-dir (math2/resize-v2 (math2/sub-v2 (:target command) (:base command)) 4.0)
            contacts (remove nil? (map (fn [[id actor]] (actor/hitpoint actor command)) actors))
            new-particles (create-particles contacts main-dir)
            new-actors (hit-actors actors sounds command)]
        (when-not (empty? contacts)
          (.play ((keyword (str "punch" (rand-int 3))) sounds))
          ;; start music at first punch, should do better but starting music needs user interaction
          (set! (.-loop (:theme sounds)) true)
          (.play (:theme sounds)))
        (-> state
            (assoc-in [:world :particles] (concat particles new-particles))
            (assoc-in [:world :actors] new-actors)))
      ;; throw dragged body
      (let [dragged ((:dragged-body sender) actors)
            masses (:masses dragged)
            newmasses (reduce (fn [res [id mass]] ; reset mass directions for next rag
                                      (assoc res id (assoc mass :d [(* 10.0 (:facing sender)) -10])))
                                    masses
                                    masses)
            newdragged (assoc dragged :masses newmasses)
            newsender (assoc sender :dragged-body nil)
            ]
        (-> state
            (assoc-in [:world :actors id] newsender)
            (assoc-in [:world :actors (:id dragged)] newdragged))))))
    

(defn calc-view-rect
  "calculates the visible rectangle of the world"
  [{{:keys [actors] :as world} :world :as state}]  
  (let [actor (:hero actors)
        [fax fay] (:p (get-in actor [:bases :base_l]))
        [fbx fby] (:p (get-in actor [:bases :base_r]))
        [tx ty] [ (+ fax (/ (- fbx fax ) 2)) (+ fay (/ (- fby fay) 2))  ]
        r (/ (.-innerWidth js/window) (.-innerHeight js/window) )
        h 350.0
        w (* h r)
        [l r b t :as new-rect] [(- tx w) (+ tx w) (+ ty h) (- ty h)]
        new-proj (math4/proj_ortho (+ l 50) (- r 50) (- b 50) (+ t 50) -1.0 1.0)]
    (-> state
        (assoc-in [:world :view-rect] new-rect)
        (assoc-in [:world :projection] new-proj))))


(defn check-ended [{:keys [commands-world] {:keys [actors finished endpos] :as world} :world :as state}]
  (let [[ex ey] endpos
        [bx by] (get-in actors [:hero :bases :base_l :p])
        dx (- bx ex)
        dy (- by ey)
        ended? (if (and (< (Math/abs dx) 50.0) (< (Math/abs dy) 50)) true false)
        new-commands (if (and (not finished) ended?) (conj commands-world {:text "next-level"}) commands-world)]
    (-> state
        (assoc :commands-world new-commands)
        (assoc-in [:world :finished] ended?))))


(defn update-particles [{{:keys [particles view-rect]} :world :as state}]
  (let [new-particles (map (fn [particle] (particle/upd particle view-rect)) particles)]
    (assoc-in state [:world :particles] new-particles)))


(defn update-guns [{{:keys [guns actors] :as world} :world :as state}]
  (let [new-guns (reduce (fn [result [id {:keys [dragged-gun] :as actor}]]
                           (if-not dragged-gun
                             result
                             (assoc result dragged-gun (actor/update-gun (dragged-gun guns) actor)))) guns actors)]
    (assoc-in state [:world :guns] new-guns)))


(defn update-dragged [{{:keys [guns actors] :as world} :world :as state}]
  (let [new-actors (reduce (fn [result [id {:keys [dragged-body] :as actor}]]
                           (if-not dragged-body
                             result
                             (assoc result dragged-body (actor/update-dragged (dragged-body result) actor)))) actors actors)]
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
                                 (assoc result id (-> actor (assoc :commands []) (assoc :action-sent true)))))
                             actors
                             actors))]
    (-> state
        (assoc :commands-world new-commands)
        (assoc-in [:world :actors] new-actors)))) 


(defn update-actors [{:keys [controls]
                      {:keys [actors surfaces guns] :as world} :world :as state}]
  (let [new-actors (reduce (fn [result [id actor]]
                             (let [newactor (cond
                                              (not= :hero id) (actor/update-actor actor nil surfaces result guns 1.0)
                                              :else (actor/update-actor actor controls surfaces result guns 1.0))]
                               
                               (assoc result id newactor)))
                           {}
                           actors)]
    (assoc-in state [:world :actors] new-actors)))


(defn update-world
  "updates phyisics and actors"
  [{{loaded :loaded} :world :as state} msg]
  (if-not loaded
    state
    (-> state
        (update-actors)
        (extract-actor-commands)
        (update-guns)
        (update-dragged)
        (check-ended)
        (calc-view-rect)
        (update-particles))))
  

(defn reset-world [{:keys [level view-rect world-drawer metrics] :as state} msg]
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
                        :guns {}
                        :infos []
                        :inited true
                        :loaded true
                        :finished false
                        :particles seeds
                        :surfaces surfaces
                        :surfacelines lines}
                       (create-actors pivots metrics))]

      (cond-> state
        true (assoc :world-drawer newdrawer)
        true (assoc :world newworld)
        true (assoc :commands-world [])
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
      (assoc :level 0)))


(defn load-next-level [{:keys [level sounds] :as state}]
  (let [next-level (min (inc level) 6)]
    (load-level! state next-level)
    (-> state
        (assoc-in [:world :loaded] false)
        (assoc :level next-level))))


(defn pickup-object [{{:keys [actors guns dragged-gun dragged-body]} :world :as oldstate} {:keys [id text]}]
  (let [actor (id actors)
        {{hip :hip} :masses color :color :as actor} actor
        ;; look for gun
        nearby-gun (first (remove nil? (map (fn [[_ {:keys [id p d]}]]
                                        (if-not (< (math2/length-v2 (math2/sub-v2 p (:p hip))) 80.0) nil id)) guns)))

        nearby-actor (first (remove nil? (map (fn [[_ {:keys [id] {ehip :hip} :masses ecolor :color ehealth :health}]]
                                          (if-not (and (< ehealth 0) (not= color ecolor) (< (math2/length-v2 (math2/sub-v2 (:p ehip) (:p hip))) 80.0)) nil id)) actors)))

        new-actor (cond-> actor
                    nearby-gun
                    (assoc :dragged-gun nearby-gun)
                    nearby-actor
                    (assoc :dragged-body nearby-actor))]

    (assoc-in oldstate [:world :actors id] new-actor)))


(defn execute-commands
  [{:keys [level commands-world ui-drawer] :as state}]
  (reduce
   (fn [oldstate {text :text :as command}]
     (println "text" text)
     (let [hero (get-in oldstate [:worlds :actors :hero])
           path-metrics [:world :actors :hero :metrics]]
       (cond

         (= text "pickup")
         (pickup-object oldstate command)

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
   commands-world))
