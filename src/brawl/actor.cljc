(ns brawl.actor
  (:require [brawl.mass :as mass]))

(defn init [x y]
  {:mode "jump"
   :a_on_ground false
   :b_on_ground false
   :speed [0.0 0.0]

   :points {:head [x y]
            :neck [x y]
            :hip  [x y]
            
            :hand_a [(+ x 10.0) y]
            :hand_b [(- x 10.0) y]
            
            :elbow_a [x y]
            :elbow_b [x y]
            
            :knee_a [x y]
            :knee_b [x y]
            
            :ankle_a [x y]
            :ankle_b [x y]
            
            :base_a [x y]
            :base_b [x y]}

   :masses {:base_a (mass/set-gravity (mass/mass2 (+ x 20.0) y 4.0 10.0 0.0) 1.0 )
            :base_b (mass/set-gravity (mass/mass2 (- x 20.0) y 4.0 10.0 0.0) 1.0 )}
   })


(defn newstate [{ :keys [ mode masses speed ] :as state } surfaces time]
  (cond
    (= mode "jump")
    (let [new_base_a (mass/set-gravity (:base_a masses) time)
          new_base_aa (mass/move-mass new_base_a surfaces time)
          new_base_b (mass/set-gravity (:base_b masses) time)
          new_base_bb (mass/move-mass new_base_b surfaces time)
          a_on_ground (every? #(= % 0.0) (:basis new_base_aa))
          b_on_ground (every? #(= % 0.0) (:basis new_base_bb))
          newmode (cond
                    (and a_on_ground b_on_ground) "walk"
                    (< (speed 0) -15) "dead"
                    :else "jump")]

    (-> state
        (assoc-in [:masses :base_a] new_base_aa)
        (assoc-in [:masses :base_b] new_base_bb)
        (assoc-in [:mode] newmode)
        (assoc-in [:a_on_ground] a_on_ground)
        (assoc-in [:b_on_ground] b_on_ground)
        ))
    (= mode "walk")
    state
    ))


(defn getpoints [ { {base_a :base_a base_b :base_b } :masses }]

  [ (:trans base_a ) (:trans base_b ) ]
  
  )
