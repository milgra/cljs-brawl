(ns brawl.actor
  (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))

(defn init [x y]
  {:mode "jump"
   :a_on_ground false
   :b_on_ground false
   
   :speed [0.0 0.0]

   :bases  {:base_a (phys2/mass2 (+ x 20.0) y 4.0 10.0 0.0)
            :base_b (phys2/mass2 (- x 20.0) y 4.0 10.0 0.0)}

   :masses {:head (phys2/mass2 x y 4.0 1.0 1.0)
            :neck (phys2/mass2 x y 4.0 1.0 1.0)
            :hip  (phys2/mass2 x y 4.0 1.0 1.0)

            :hand_a (phys2/mass2 x y 4.0 1.0 1.0)
            :hand_b (phys2/mass2 x y 4.0 1.0 1.0)
            
            :elbow_a (phys2/mass2 x y 4.0 1.0 1.0)
            :elbow_b (phys2/mass2 x y 4.0 1.0 1.0)
            
            :knee_a (phys2/mass2 x y 4.0 1.0 1.0)
            :knee_b (phys2/mass2 x y 4.0 1.0 1.0)
            
            :ankle_a (phys2/mass2 x y 4.0 1.0 1.0)
            :ankle_b (phys2/mass2 x y 4.0 1.0 1.0)}})


(defn triangle_with_bases [va vb side dir]
  (let [dirv (math2/sub-v2 vb va)
        half (math2/scale-v2 dirv 0.5)
        size (math2/length-v2 half) 
        result (if (< size side)
                 (let [needed (Math/sqrt (- (* side side) (* size size)))
                       normal (math2/resize-v2 [(* dir (- (half 1))) (* dir (half 0))] needed)]
                   (math2/add-v2 (math2/add-v2 va half) normal))
                 (math2/add-v2 va half))]
    result))


(defn updateskeleton [ {{{[txa tya] :p} :base_a
                         {[txb tyb] :p} :base_b} :bases :as  state }]
  (let [ankle_a [txa tya]
        ankle_b [txb tyb]
        hipx (+ txa (/ (- txb txa) 2))
        hipy (- (+ tya (/ (- tyb tya) 2)) 50.0)
        neck [hipx (- hipy 50.0)]
        head [hipx (- hipy 70.0)]
        knee_a (triangle_with_bases ankle_a [hipx hipy] 30.0 1.0)
        knee_b (triangle_with_bases ankle_b [hipx hipy] 30.0 1.0)
        hand_a [(+ hipx 40.0) (- hipy 50.0)]
        hand_b [(+ hipx 30.0) (- hipy 40.0)]
        elbow_a (triangle_with_bases neck hand_a 30.0 1.0)
        elbow_b (triangle_with_bases neck hand_b 30.0 1.0)]

  (-> state
      (assoc-in [:masses :ankle_a :p] ankle_a) 
      (assoc-in [:masses :ankle_b :p] ankle_b) 
      (assoc-in [:masses :knee_a :p] knee_a ) 
      (assoc-in [:masses :knee_b :p] knee_b ) 
      (assoc-in [:masses :hip :p] [hipx hipy])
      (assoc-in [:masses :neck :p] neck)
      (assoc-in [:masses :head :p] head)
      (assoc-in [:masses :hand_a :p] hand_a) 
      (assoc-in [:masses :hand_b :p] hand_b) 
      (assoc-in [:masses :elbow_a :p] elbow_a) 
      (assoc-in [:masses :elbow_b :p] elbow_b))))


(defn newstate [{:keys [mode bases masses speed] :as state } surfaces time]
  (cond
    (= mode "jump")
    (let [newbases (-> bases
                       (phys2/add-gravity [0.0 0.5])
                       (phys2/move-masses surfaces))
          a_on_ground (every? #(= % 0.0) (:d (:base_a newbases)))
          b_on_ground (every? #(= % 0.0) (:d (:base_b newbases)))
          newmode (cond
                    (and a_on_ground b_on_ground) "walk"
                    (< (speed 0) -15) "dead"
                    :else "jump")]
      
      (-> state
          (assoc :bases newbases)
          (assoc-in [:mode] newmode)
          (assoc-in [:a_on_ground] a_on_ground)
          (assoc-in [:b_on_ground] b_on_ground)
          (updateskeleton)))
    (= mode "walk")
    state
    ))
  

(defn getpoints [{masses :masses bases :bases}]
  (concat (map :p (vals masses)) (map :p (vals bases))))


(defn getlines [{{:keys [head neck hip elbow_a elbow_b hand_a hand_b knee_a knee_b ankle_a ankle_b]} :masses}]
  [(:p head) (:p neck)
   (:p neck) (:p hip)
   (:p hip) (:p knee_a)
   (:p hip) (:p knee_b)
   (:p knee_a ) (:p ankle_a)
   (:p knee_b ) (:p ankle_b)
   (:p neck ) (:p elbow_a)
   (:p neck ) (:p elbow_b)
   (:p elbow_a ) (:p hand_a)
   (:p elbow_b ) (:p hand_b)])


(defn gen-tube-triangles [ points ]
  (reduce
   (fn [result [pa pb]]
     (let [ab (math2/sub-v2 pa pb)
           n1 (math2/resize-v2 (math2/rotate-90-cw ab) 10.0)
           n2 (math2/resize-v2 (math2/rotate-90-ccw ab) 10.0)]

       (conj result
             (math2/add-v2 pa n1)
             (math2/add-v2 pa n2)
             (math2/add-v2 pb n1)
             (math2/add-v2 pa n2)
             (math2/add-v2 pb n2)
             (math2/add-v2 pb n1))))
   []
   (partition 2 1 points)))


(defn get-skin-triangles [{{:keys [head neck hip elbow_a elbow_b hand_a hand_b knee_a knee_b ankle_a ankle_b]} :masses}]
  (gen-tube-triangles [(:p head) (:p neck) (:p hip) (:p knee_a) (:p ankle_a)]))
