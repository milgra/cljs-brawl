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
       (filter (fn [[dx dy id]] (and (< dx 500) (< dy 200)))) ;; filter far away actors
       (map (fn [[dx dy id]] [(+ dx dy) id])) ;; map to distance sum
       (sort-by first))) ;; sort by distance sum


(defn collect-bodies
  "collect dead actors"
  [id actors [x y]]
  (->> (vals actors)
       (filter #(= (:mode %) :idle)) ;; filter dead actors
       (filter #(<= (:health %) 0)) ;; filter dead actors
       (map (fn [{{{[ex ey] :p} :head} :masses id :id}] [(Math/abs (- ex x)) (Math/abs (- ey y)) id])) ;; extract distance
       (filter (fn [[dx dy id]] (and (< dx 500) (< dy 500)))) ;; filter far away actors
       (map (fn [[dx dy id]] [(+ dx dy) id])) ;; map to distance sum
       (sort-by first))) ;; sort by distance sum


(defn follow-hero?
  "check if hero is close enough"
  [actor actors]
  (let [{:keys [color]} actor
        {:keys [arml legl]} (:metrics actor)
        {{pos :p} :head} (:masses actor)]
    (if-not (= color 0xFF0000FF)
      false
      (let [hero (:hero actors)
            health (:health hero)
            pos-actor (get-in hero [:masses :head :p])
            [dx dy] (math2/sub-v2 pos-actor pos)]
         (if (and (< (Math/abs dx) 500)
                  (< (Math/abs dy) 500)
                  (> (Math/abs dx) arml)
                  (> health 0)) true false)))))


(defn look-for-target
  "check for enemy to attack, hero to follow, or set attacker as target"
  [actor actors time]
  (let [{:keys [color id]} actor
        {:keys [body]} (:drag actor)
        {:keys [timeout attacker state]} (:ai actor)
        {{[x y :as pos] :p} :head} (:masses actor)]
    (if-not (> time timeout)
      actor ;; do nothing before timeout
      (if (and attacker (> 0 (:health (attacker actors))))
        ;; if we were attacked before
        (-> actor
            (assoc-in [:ai :target] attacker) ;; set target
            (assoc-in [:ai :attacker] nil) ;; reset attacker
            (assoc-in [:ai :state] :follow)) ;; set follow mode
        ;; look for enemies
        (let [enemies (collect-enemies id actors color pos) ;; get nearby enemies
              target (if-not (empty? enemies) ;; look for enemy target
                       (second (first enemies))
                       (let [bodies (collect-bodies id actors pos)] ;; look for dead body target
                         (if (and (not body) (not (empty? bodies)))
                           (second (first bodies))
                           (if (follow-hero? actor actors) :hero nil))))] ;; or follow hero

          (if target
            ;; follow target
            (-> actor
                (assoc-in [:ai :target] target) ;; set target
                (assoc-in [:ai :attacker] nil) ;; reset attacker
                (assoc-in [:ai :state] :follow)) ;; set follow mode
            ;; stay
            (-> actor
                (assoc-in [:ai :attacker] nil) ;; reset attacker
                (assoc-in [:ai :timeout] (+ time 100 (rand-int 100)))) ;; reset attacker
            ))))))


(defn follow-target
  "go after enemy/hero/body and attack/idle/pickup"
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
          reached  (and (< x (+ px arml)) (> x (- px arml)))
          still-actor (-> actor
                          (assoc-in [:attack :pickup-sent] false)
                          (assoc-in [:attack :action-sent] false)
                          (assoc-in [:control :run] false)
                          (assoc-in [:control :down] false)
                          (assoc-in [:control :up] false)
                          (assoc-in [:control :punch] false)
                          (assoc-in [:control :block] false)
                          (assoc-in [:control :kick] false)
                          (assoc-in [:control :left] false)
                          (assoc-in [:control :right] false))]
      (if reached
        ;; start attack if target is enemy or idle if target is hero
        (cond
          (and (= color 0xFF0000FF) (= (:id enemy) :hero))
          ;; reached hero, stay
          (-> still-actor
              (assoc-in [:ai :state] :idle) ;; after follow sequence check for enemies again
              (assoc-in [:ai :target] :nil)
              (assoc [:ai :timeout] (+ time 100)))
          
          (and (< (:health enemy) 0) (not body))
          ;; reached body, pickup
          (-> still-actor
              (assoc-in [:ai :state] :idle)
              (assoc-in [:control :down] true)
              (assoc-in [:ai :timeout] (+ time 100)))
          ;; reached enemy, attack
          :else
          (-> still-actor
              (assoc-in [:ai :state] :attack)
              (assoc-in [:ai :timeout] (+ time 100))))
        ;; follow target
        (let [new-direction (cond
                              (> py (+ y 20.0)) -1
                              (< py (- y 20.0)) 1
                              :else direction)]
          (cond-> still-actor
            true (assoc [:ai :timeout] (+ time 100))
            true (assoc-in [:walk :direction] new-direction) 
            true (assoc-in [:control :run] true)
            (<= x (- px arml)) (assoc-in [:control :right] true)
            (<= x (- px arml)) (assoc-in [:control :left] false)
            (>= x (+ px arml)) (assoc-in [:control :left] true)
            (>= x (+ px arml)) (assoc-in [:control :right] false))))))))
  

(defn attack-target
  "attack enemy"
  [actor control surfaces actors time delta]
  (let [{:keys [id color level]} actor
        {:keys [timeout target]} (:ai actor)
        {{pos :p} :head} (:masses actor)
        pick (rand-int 4)]
    (if-not (> time timeout)
      actor
      (cond-> actor
        (= pick 0) (assoc-in [:control :punch] true)
        (= pick 1) (assoc-in [:control :kick] true)
        (= pick 2) (assoc-in [:control :block] true)
        (= pick 3) (assoc-in [:control :down] true)
        true (assoc-in [:ai :state] :follow)
        true (assoc-in [:ai :target] target)
        true (assoc-in [:ai :timeout] (+ time 200))))))


;; after every action ai should reconsider finding new enemy, finding dead body, following, etc
(defn update-ai
  [actor control surfaces actors time delta]
  (let [{{state :state} :ai} actor]
    (if control
      actor
      (case state
        :idle (look-for-target actor actors time)
        :follow (follow-target actor control surfaces actors time delta)
        :attack (attack-target actor control surfaces actors time delta)
        actor
      ;;  :idle (update-idle actor actors time)
      ;;  :follow (update-follow actor control surfaces actors time delta)
      ;;  :attack (update-attack actor control surfaces actors time delta))
      ))))

