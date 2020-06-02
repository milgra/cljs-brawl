(ns brawl.actorai
    (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))


(defn update-ai
  [{:keys [id color ai-state ai-timeout ai-enemy] {{[x y :as pos] :p} :base_l} :bases :as state} control surfaces actors time delta]
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
                (assoc :ai-enemy (if enemy (second enemy) nil))
                (assoc :ai-timeout 0)
                (assoc :ai-state :follow))
            (assoc state :ai-timeout (+ ai-timeout 1000)))))
      (= :follow ai-state)
      (if-not (> time ai-timeout)
        state
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
            (and (< x (+ px 50.0)) (> x (- px 50.0))) (assoc :ai-timeout (+ ai-timeout 500))
            (and (< x (+ px 50.0)) (> x (- px 50.0))) (assoc-in [:control :punch] true))))
      (= :attack ai-state)
      ;;(println "attack" (:control state) new-duration)
      (if-not (> time ai-timeout)
        state
        (-> state
          (assoc :ai-state :follow)
          (assoc :action-sent false)
          (assoc-in [:control :punch] false)
          (assoc :ai-timeout (+ ai-timeout 500)))))))


