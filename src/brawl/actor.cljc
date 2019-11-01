(ns brawl.actor
  (:require [brawl.mass :as mass]))

(defn init [x y]
  {:mode "jump"
   :a_on_ground false
   :b_on_ground false
   
   :speed [0.0 0.0]

   :masses {:base_a (mass/set-gravity (mass/mass2 (+ x 20.0) y 4.0 10.0 0.0) 1.0 )
            :base_b (mass/set-gravity (mass/mass2 (- x 20.0) y 4.0 10.0 0.0) 1.0 )

            :head (mass/mass2 x y 4.0 1.0 1.0)
            :neck (mass/mass2 x y 4.0 1.0 1.0)
            :hip  (mass/mass2 x y 4.0 1.0 1.0)
            
            :hand_a (mass/mass2 x y 4.0 1.0 1.0)
            :hand_b (mass/mass2 x y 4.0 1.0 1.0)
            
            :elbow_a (mass/mass2 x y 4.0 1.0 1.0)
            :elbow_b (mass/mass2 x y 4.0 1.0 1.0)
            
            :knee_a (mass/mass2 x y 4.0 1.0 1.0)
            :knee_b (mass/mass2 x y 4.0 1.0 1.0)
            
            :ankle_a (mass/mass2 x y 4.0 1.0 1.0)
            :ankle_b (mass/mass2 x y 4.0 1.0 1.0)}})


(defn updateskeleton [ {{{[txa tya] :trans} :base_a
                         {[txb tyb] :trans} :base_b} :masses :as  state }]
  (let [hipx (+ txa (/ (- txb txa) 2))
        hipy (- (+ tya (/ (- tyb tya) 2)) 50.0)]
  (-> state
      (assoc-in [:masses :ankle_a :trans] [txa tya]) 
      (assoc-in [:masses :ankle_b :trans] [txb tyb]) 
      (assoc-in [:masses :knee_a :trans] [txa (- tya 20.0)]) 
      (assoc-in [:masses :knee_b :trans] [txb (- tyb 20.0)]) 
      (assoc-in [:masses :hip :trans] [hipx hipy])
      (assoc-in [:masses :neck :trans] [hipx (- hipy 50.0)])
      (assoc-in [:masses :head :trans] [hipx (- hipy 70.0)])
      (assoc-in [:masses :hand_a :trans] [(+ hipx 40.0) (- hipy 50.0)]) 
      (assoc-in [:masses :hand_b :trans] [(+ hipx 30.0) (- hipy 40.0)]) 
      (assoc-in [:masses :elbow_a :trans] [(+ hipx 20.0) (- hipy 40.0)]) 
      (assoc-in [:masses :elbow_b :trans] [(+ hipx 10.0) (- hipy 30.0)]) 
      )))


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
        (updateskeleton)
        ))
    (= mode "walk")
    state
    ))
  


(defn getpoints [{masses :masses}]
  (map :trans (vals masses )))
