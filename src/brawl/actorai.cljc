(ns brawl.actorai
  (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))


(defn collect-enemies
  "collects living actors with different colors"
  [id actors color [x y]]
  (->> (vals actors)
       (filter #(not= (:color %) color)) ;; filter actors with different colors
       (filter #(> (:health %) 0)) ;; filter dead actors
       (map (fn [{{{[ex ey] :p} :head} :masses id :id}] [(Math/abs (- ex x)) (Math/abs (- ey y)) id])) ;; extract distance
       (filter (fn [[dx dy id]] (and (< dx 500) (< dy 500)))) ;; filter far away actors
       (map (fn [[dx dy id]] [(+ dx dy) id])) ;; map to distance sum
       (sort-by first))) ;; sort by distance sum


(defn collect-bodies
  "collect dead actors"
  [id actors [x y]]
  (->> (vals actors)
       (filter #(<= (:health %) 0)) ;; filter dead actors
       (map (fn [{{{[ex ey] :p} :head} :masses id :id}] [(Math/abs (- ex x)) (Math/abs (- ey y)) id])) ;; extract distance
       (filter (fn [[dx dy id]] (and (< dx 500) (< dy 500)))) ;; filter far away actors
       (map (fn [[dx dy id]] [(+ dx dy) id])) ;; map to distance sum
       (sort-by first))) ;; sort by distance sum


(defn follow-hero?
  "check if hero is close enough"
  [actor actors]
  (let [{:keys [color]} actor
        {{pos :p} :head} (:masses actor)]
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
                 (> health 0)) true false)))))


(defn update-idle
  "look for enemy, if there's no enemy and if our color is red look for hero to follow"
  [actor actors time]
  (let [{:keys [color id]} actor
        {:keys [timeout]} (:ai actor)
        {{pos :p} :head} (:masses actor)]
    (if-not (> time timeout)
      actor
      (let [enemies (collect-enemies id actors color pos) ;; get nearby enemies
            target (if-not (empty? enemies)
                     (second (first enemies))
                     (if (follow-hero? actor actors) :hero nil))] ;; or follow hero
        (if target
          ;; change to follow state
          (-> actor
              (assoc-in [:attack :pickup-sent] false)
              (assoc-in [:ai :target] target)
              (assoc-in [:ai :timeout] (+ time 100 (rand-int 100)))
              (assoc-in [:ai :state] :follow))
          ;; if no target re-check everything after 1 sec
          (-> actor
              (assoc-in [:control :down] false)
              (assoc-in [:control :punch] false)
              (assoc-in [:control :kick] false)
              (assoc-in [:control :block] false)
              (assoc-in [:ai :timeout] (+ time 200 (rand-int 200)))))))))


(defn attack
  [actor actors time]
  (let [{:keys [color]} actor
        {:keys [target]} (:ai actor)
        enemy (target actors)
        health (:health enemy)
        pick (rand-int 3)
        dead (<= health 0)
        newactor (-> actor
                     (assoc-in [:control :left] false)
                     (assoc-in [:control :right] false)
                     (assoc-in [:ai :timeout] (+ time 200)))]
    (if (and (= color 0xFF0000FF) (= target :hero))
      (assoc-in newactor [:ai :state] :idle)
      (if (and dead (not= target :hero))
        ;; pick up body, in next idle state we will find our enemy
        (-> newactor
            (assoc-in [:ai :state] :idle)
            (assoc-in [:control :down] true)
            (assoc-in [:ai :timeout] (+ time 100)))
        ;; attack enemy
        (cond-> newactor
          true (assoc-in [:ai :timeout] (+ time 100))
          true (assoc-in [:ai :state] :attack)
          true (assoc-in [:control :down] (if (= (rand-int 2) 0) true false))
          true (assoc-in [:control :down] false)
          (= pick 0) (assoc-in [:control :punch] true)
          (= pick 1) (assoc-in [:control :kick] true)
          (= pick 2) (assoc-in [:control :block] true))))))


(defn update-follow
  "go after enemy/hero"
  [actor control surfaces actors time delta]
  (let [{:keys [id color]} actor
        {:keys [body]} (:drag actor)
        {:keys [direction]} (:walk actor)
        {:keys [arml legl]} (:metrics actor)
        {:keys [state timeout target]} (:ai actor)
        {{[x y :as pos] :p} :head} (:masses actor)]
  (if-not (> time timeout)
    actor
    (let [enemy (target actors)
          [px py] (get-in enemy [:masses :head :p])
          reached  (and (< x (+ px arml)) (> x (- px arml)))]
      (if reached
        ;; start attack if target is enemy or idle if target is hero
        (attack actor actors time)
        ;; follow target
        (let [new-direction (cond
                              (> py (+ y 20.0)) -1
                              (< py (- y 20.0)) 1
                              :else direction)]
          (cond-> actor
            true (assoc [:ai :timeout] (+ time 100))
            true (assoc-in [:walk :direction] new-direction) 
            (<= x (- px arml)) (assoc-in [:control :right] true)
            (<= x (- px arml)) (assoc-in [:control :left] false)
            (>= x (+ px arml)) (assoc-in [:control :left] true)
            (>= x (+ px arml)) (assoc-in [:control :right] false)
            body (assoc-in [:control :punch] true))))))))
        

(defn update-attack
  "keep last move until timeout"
  [actor control surfaces actors time delta]
  (let [{:keys [id level]} actor
        {:keys [timeout target]} (:ai actor)
        {{pos :p} :head} (:masses actor)]
    (if-not (> time timeout)
      actor
      (let [bodies (collect-bodies id actors pos) ;; get nearby dead bodies
            target (if-not (empty? bodies)
                     (second (first bodies)))
            newactor (-> actor
                         (assoc-in [:attack :action-sent] false)
                         (assoc-in [:control :punch] false)
                         (assoc-in [:control :kick] false)
                         (assoc-in [:control :block] false))]
        (if (and target (= 1 (rand-int 3)))
           (-> newactor
              (assoc-in [:ai :state] :follow)
              (assoc-in [:ai :target] target)
              (assoc-in [:ai :timeout] (+ time 200)))
          (-> newactor
              (assoc-in [:ai :state] :follow)
              (assoc-in [:ai :timeout] (+ time 400 (rand-int 100) (* level -30)))))))))


;; after every action ai should reconsider finding new enemy, finding dead body, following, etc
(defn update-ai
  [actor control surfaces actors time delta]
  (let [{{state :state} :ai} actor]
    (if control
      actor
      (case state
        :idle (update-idle actor actors time)
        :follow (update-follow actor control surfaces actors time delta)
        :attack (update-attack actor control surfaces actors time delta)))))

