(ns brawl.actorai
    (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))


(defn collect-enemies [id actors color [x y]]
  (->> (vals actors)
       (filter #(not= (:color %) color)) ;; filter actors with different colors
       (filter #(> (:health %) 0)) ;; filter dead actors
       (map (fn [{{{[ex ey] :p} :head} :masses id :id :as actor}] [(Math/abs (- ex x)) (Math/abs (- ey y)) id])) ;; extract distance
       (filter (fn [[dx dy id]] (and (< dx 500) (< dy 500)))) ;; filter far away actors
       (map (fn [[dx dy id]] [(+ dx dy) id])) ;; map to distance sum
       (sort-by first))) ;; sort by distance sum


(defn follow-hero? [{:keys [color] {{pos :p} :head} :masses :as state} actors]
  (if-not (= color 0xFF0000FF)
    false
    (let [actor (:hero actors)
          health (:health actor)
          pos-actor (get-in actor [:masses :head :p])
          [dx dy] (math2/sub-v2 pos-actor pos)]
      (if (and (< (Math/abs dx) 500)
               (< (Math/abs dy) 500)
               (> (Math/abs dx) 50)
               (> (Math/abs dx) 50)
               (> health 0)) true false))))


(defn update-idle
  "look for enemy, if there's no enemy and if our color is red look for hero to follow"
  [{:keys [color ai-timeout id]
    {{pos :p} :head} :masses :as state}
   actors
   time]
  (if-not (> time ai-timeout)
    state
    (let [enemies (collect-enemies id actors color pos) ;; get nearby enemies
          target (if-not (empty? enemies)
                     (second (first enemies))
                     (if (follow-hero? state actors) :hero nil))] ;; or follow hero
      (if target
        ;; change to follow state
        (-> state
            (assoc :ai-enemy target)
            (assoc :ai-timeout 0)
            (assoc :ai-state :follow))
        ;; if no target re-check everything after 1 sec
        (assoc state :ai-timeout (+ time 1000))))))


(defn update-follow
  "go after enemy/hero"
  [{:keys [color ai-state ai-timeout ai-enemy]
    {{[x y :as pos] :p} :head} :masses
    {:keys [arml legl]} :metrics
    :as state}
   control
   surfaces
   actors
   time
   delta]
  (if-not (> time ai-timeout)
    state
    (let [enemy (ai-enemy actors)
          health (:health enemy)
          [px py] (get-in enemy [:bases :base_l :p])
          reached  (and (< x (+ px arml)) (> x (- px arml)) (> health 0))
          dead (<= health 0)
          pick (rand-int 3)]
      (cond
        ;; start attack if target is enemy or idle if target is hero
        reached (let [newstate (-> state
                                   (assoc-in [:control :left] false)
                                   (assoc-in [:control :right] false)
                                   (assoc :ai-timeout (+ time 200)))]
                  (if (and (= color 0xFF0000FF) (= ai-enemy :hero))
                    (assoc newstate :ai-state :idle)
                    (cond-> newstate
                        true (assoc :ai-state :attack)
                        true (assoc-in [:control :down] (if (= (rand-int 2) 0) true false))
                        (= pick 0) (assoc-in [:control :punch] true)
                        (= pick 1) (assoc-in [:control :kick] true)
                        (= pick 2) (assoc-in [:control :block] true))))
        ;; stop attack if target is dead
        dead (-> state
                 (assoc :ai-state :idle)
                 (assoc-in [:control :punch] false)
                 (assoc-in [:control :kick] false)
                 (assoc-in [:control :block] false)
                 (assoc-in [:control :down] false))
        ;; follow target
        :else (cond-> state
                (<= x (- px arml)) (assoc-in [:control :right] true)
                (<= x (- px arml)) (assoc-in [:control :left] false)
                (>= x (+ px arml)) (assoc-in [:control :left] true)
                (>= x (+ px arml)) (assoc-in [:control :right] false))))))
        


(defn update-pickup
  "go after dead body to pickup"
  [{:keys [id color ai-state ai-timeout ai-enemy]
    {{[x y :as pos] :p} :head} :masses
    {:keys [arml legl]} :metrics
    :as state}
   control
   surfaces
   actors
   time
   delta]
  state)


(defn update-attack
  "keep last move until timeout"
  [{:keys [id color ai-state ai-timeout ai-enemy]
    {{[x y :as pos] :p} :head} :masses
    {:keys [arml legl]} :metrics
    :as state}
   control
   surfaces
   actors
   time
   delta]
  (let [enemy (ai-enemy actors)]
    (if-not (> time ai-timeout)
      state
      (-> state
          (assoc :ai-state :follow)
          (assoc :action-sent false)
          (assoc-in [:control :punch] false)
          (assoc-in [:control :kick] false)
          (assoc-in [:control :block] false)
          (assoc :ai-timeout (+ time 200))))))


;; after every action ai should reconsider finding new enemy, finding dead body, following, etc
(defn update-ai
  [{:keys [id color ai-state ai-timeout ai-enemy] {{[x y :as pos] :p} :head} :masses
    {:keys [arml legl]} :metrics
    :as state} control surfaces actors time delta]
  (if control
    state
    (cond
      (= :idle ai-state) (update-idle state actors time)
      (= :follow ai-state) (update-follow state control surfaces actors time delta)
      (= :pickup ai-state) (update-pickup state control surfaces actors time delta)
      (= :attack ai-state) (update-attack state control surfaces actors time delta))))

