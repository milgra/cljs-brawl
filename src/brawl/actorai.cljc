(ns brawl.actorai
    (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))


;; after every action ai should reconsider finding new enemy, finding dead body, following, etc
(defn update-ai
  [{:keys [id color ai-state ai-timeout ai-enemy] {{[x y :as pos] :p} :head} :masses
    {:keys [arml legl]} :metrics
    :as state} control surfaces actors time delta]
  (if control
    state
    (cond
      (= :idle ai-state)
      (if-not (> time ai-timeout)
        state
        (let [by-distance (sort-by
                           first
                           (remove
                            nil?
                            (map (fn [[aid actor]]
                                   (let [pos-enemy (get-in actor [:masses :head :p])
                                         health (:health actor)
                                         col (:color actor)
                                         [dx dy] (math2/sub-v2 pos-enemy pos)]
                                     (if (= aid :hero) (println id color "enemy?" aid col dx dy))
                                     (if (and (< (Math/abs dx) 500) (< (Math/abs dy) 500) (not= col color) (> health 0)) [(+ dx dy) aid] nil)))
                                 actors)))
              enemy (first by-distance)]
          ;; look for enemy
          (if enemy
            (-> state
                (assoc :ai-enemy (if enemy (second enemy) nil))
                (assoc :ai-timeout 0)
                (assoc :ai-state :follow))
            (assoc state :ai-timeout (+ time 1000)))))
      (= :follow ai-state)
      (if-not (> time ai-timeout)
        state
        (let [enemy (ai-enemy actors)
              health (:health enemy)
              [px py] (get-in enemy [:bases :base_l :p])
              reached  (and (< x (+ px arml)) (> x (- px arml)))
              dead (<= health 0)
              pick (rand-int 3)]
          (cond-> state
            (<= x (- px arml)) (assoc-in [:control :right] true)
            (<= x (- px arml)) (assoc-in [:control :left] false)
            (>= x (+ px arml)) (assoc-in [:control :left] true)
            (>= x (+ px arml)) (assoc-in [:control :right] false)
            reached (assoc-in [:control :left] false)
            reached (assoc-in [:control :right] false)
            reached (assoc :ai-state :attack)
            reached (assoc :ai-timeout (+ time 200))
            dead (assoc :ai-state :idle)
            dead (assoc-in [:control :punch] false)
            dead (assoc-in [:control :kick] false)
            dead (assoc-in [:control :block] false)
            dead (assoc-in [:control :down] false)
            (and reached (not dead)) (assoc-in [:control :down] (if (= (rand-int 2) 0) true false))
            (and reached (= pick 0) (not dead)) (assoc-in [:control :punch] true)
            (and reached (= pick 1) (not dead)) (assoc-in [:control :kick] true)
            (and reached (= pick 2) (not dead)) (assoc-in [:control :block] true))))
      (= :attack ai-state)
      (let [enemy (ai-enemy actors)]
        (if-not (> time ai-timeout)
          state
          (-> state
              (assoc :ai-state :follow)
              (assoc :action-sent false)
              (assoc-in [:control :punch] false)
              (assoc-in [:control :kick] false)
              (assoc-in [:control :block] false)
              (assoc :ai-timeout (+ time 200))))))))


