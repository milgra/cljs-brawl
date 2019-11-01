(ns brawl.actor
  (:require [brawl.mass :as mass]
            [brawl.math2 :as math2 ]))

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
  (let [ankle_a [txa tya]
        ankle_b [txb tyb]
        hipx (+ txa (/ (- txb txa) 2))
        hipy (- (+ tya (/ (- tyb tya) 2)) 50.0)
        neck [hipx (- hipy 50.0)]
        head [hipx (- hipy 70.0)]
        knee_a (math2/triangle_with_bases ankle_a [hipx hipy] 30.0 1.0)
        knee_b (math2/triangle_with_bases ankle_b [hipx hipy] 30.0 1.0)
        hand_a [(+ hipx 40.0) (- hipy 50.0)]
        hand_b [(+ hipx 30.0) (- hipy 40.0)]
        elbow_a (math2/triangle_with_bases neck hand_a 30.0 1.0)
        elbow_b (math2/triangle_with_bases neck hand_b 30.0 1.0)]
  (println "masses" (:masses state))
  (-> state
      (assoc-in [:masses :ankle_a :trans] ankle_a) 
      (assoc-in [:masses :ankle_b :trans] ankle_b) 
      (assoc-in [:masses :knee_a :trans] knee_a ) 
      (assoc-in [:masses :knee_b :trans] knee_b ) 
      (assoc-in [:masses :hip :trans] [hipx hipy])
      (assoc-in [:masses :neck :trans] neck)
      (assoc-in [:masses :head :trans] head)
      (assoc-in [:masses :hand_a :trans] hand_a) 
      (assoc-in [:masses :hand_b :trans] hand_b) 
      (assoc-in [:masses :elbow_a :trans] elbow_a) 
      (assoc-in [:masses :elbow_b :trans] elbow_b) )))


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
