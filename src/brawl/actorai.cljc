(ns brawl.actorai
    (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))


(defn update-ai
  [{:keys [id color ai-state ai-duration ai-enemy] {{[x y :as pos] :p} :base_l} :bases :as state} control surfaces actors time]
  (if-not control
    (let [new-duration (+ ai-duration time)]
      (cond
        (= :idle ai-state)
        (if (> new-duration 60)
          (let [by-distance (sort-by
                             first
                             (remove
                              nil?
                              (map (fn [[id actor]]
                                     (let [pos-enemy (get-in actor [:bases :base_l :p])
                                           col (:color actor)
                                           [dx dy] (math2/sub-v2 pos-enemy pos)]
                                       (if (and (< (Math/abs dx) 500) (< (Math/abs dy) 500) (not= col color)) [(+ dx dy) id] nil)))
                                   actors)))
                enemy (first by-distance)]
            ;; look for enemy
            (if enemy
              (-> state
                  (assoc :ai-duration 0)
                  (assoc :ai-enemy (if enemy (second enemy) nil))
                  (assoc :ai-state :follow))
              (assoc state :ai-duration 0)))
          (assoc state :ai-duration new-duration))
        (= :follow ai-state)
        (let [enemy (ai-enemy actors)
              [px py] (get-in enemy [:bases :base_l :p])]
          ;;(println "follow" (:control state))
          (cond-> state
             (< x (- px 50.0)) (assoc-in [:control :right] true)
             (< x (- px 50.0)) (assoc-in [:control :left] false)
             (> x (+ px 50.0)) (assoc-in [:control :left] true)
             (> x (+ px 50.0)) (assoc-in [:control :right] false)
             (and (< x (+ px 50.0)) (> x (- px 50.0))) (assoc-in [:control :left] false)
             (and (< x (+ px 50.0)) (> x (- px 50.0))) (assoc-in [:control :right] false)
             (and (< x (+ px 50.0)) (> x (- px 50.0))) (assoc :ai-state :attack)
             (and (< x (+ px 50.0)) (> x (- px 50.0))) (assoc :ai-duration 0)
             (and (< x (+ px 50.0)) (> x (- px 50.0))) (assoc-in [:control :punch] true)))
        (= :attack ai-state)
        (do
          ;;(println "attack" (:control state) new-duration)
          (cond
            (> new-duration 30)
            (-> state
                (assoc :ai-state :follow))
            (> new-duration 15)
            (-> state
                (assoc :action-sent false)
                (assoc-in [:control :punch] false)
                (assoc :ai-duration new-duration))
            :else (assoc state :ai-duration new-duration)))
        :else
        (assoc state :ai-duration new-duration)))
    state))

