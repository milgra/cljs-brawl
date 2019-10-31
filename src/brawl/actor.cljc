(ns brawl.actor
  (:require [brawl.mass :as mass]))

(defn init [x y]
  {:mode "jump"

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

   :masses {
            :base_a (mass/mass2 x y)
            :base_b (mass/mass2 x y)
            }
   })


(defn newstate [{ :keys [ mode masses ] :as state } surfaces time]
  (cond
    (= mode "jump")
    (let [new_base_a (mass/set-gravity (:base_a masses) time)
          new_base_aa (mass/move-mass new_base_a surfaces time)
          new_base_b (mass/set-gravity (:base_b masses) time)
          new_base_bb (mass/move-mass new_base_b surfaces time)]
    (-> state
        (assoc-in [:masses :base_a] new_base_aa)
        (assoc-in [:masses :base_b] new_base_bb)
    ))))

(defn getpoints [ { {base_a :base_a base_b :base_b } :masses }]

  [ (:trans base_a ) (:trans base_b ) ]
  
  )
