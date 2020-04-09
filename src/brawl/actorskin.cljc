(ns brawl.actorskin
  (:use [mpd.math2 :only [resize-v2 scale-v2 rotate-90-cw rotate-90-ccw add-v2 sub-v2]]))

(defn getpoints [{masses :masses}]
  (concat (map :p (vals masses))))


(defn getlines [{{:keys [head neck hip elbow_l elbow_r hand_l hand_r knee_l knee_r foot_l foot_r]} :masses step-zone :step-zone}]
  [(:p head) (:p neck)
   (:p neck) (:p hip)
   (:p hip) (:p knee_l)
   (:p hip) (:p knee_r)
   (:p knee_l ) (:p foot_l)
   (:p knee_r ) (:p foot_r)
   (:p neck ) (:p elbow_l)
   (:p neck ) (:p elbow_r)
   (:p elbow_l ) (:p hand_l)
   (:p elbow_r ) (:p hand_r)
   (:A step-zone) (:B step-zone)
   (:A step-zone) (:C step-zone)])
  

(defn gen-tube-triangles [points sizes]
  (loop [rempts points
         remszs sizes
         result []]
    (if (= 2 (count rempts))
      ;; close tube
      (let [pa (nth rempts 0)
            pb (nth rempts 1)
            sa (nth remszs 0)
            sb (nth remszs 1)
            ab (sub-v2 pb pa)
            nlsa (add-v2 pa (resize-v2( rotate-90-ccw ab) sa))
            nrsa (add-v2 pa (resize-v2( rotate-90-cw ab) sa))
            nlea (add-v2 pb (resize-v2( rotate-90-ccw ab) sb))
            nrea (add-v2 pb (resize-v2( rotate-90-cw ab) sb))]
        (conj result
              nlsa nrsa nrea
              nlsa nrea nlea))
      ;; add rect and joint triangle
      (let [pa (nth rempts 0)
            pb (nth rempts 1)
            pc (nth rempts 2)
            sa (nth remszs 0)
            sb (nth remszs 1)
            ab (sub-v2 pb pa)
            bc (sub-v2 pc pb)
            nlsa (add-v2 pa (resize-v2( rotate-90-ccw ab) sa))
            nrsa (add-v2 pa (resize-v2( rotate-90-cw ab) sa))
            nlea (add-v2 pb (resize-v2( rotate-90-ccw ab) sb))
            nrea (add-v2 pb (resize-v2( rotate-90-cw ab) sb))
            nlsb (add-v2 pb (resize-v2( rotate-90-ccw bc) sb))
            nrsb (add-v2 pb (resize-v2( rotate-90-cw bc) sb))]
        (recur (rest rempts)
               (rest remszs)
               (conj result
                     nlsa nrsa nrea
                     nlsa nrea nlea
                     nlea nlsb pb
                     nrea nrsb pb))))))


(defn gen-foot-triangles [pa pb size facing]
  (let [ab (resize-v2 (sub-v2 pb pa) 20.0)
        abbig pb ;;(add-v2 pb (scale-v2 ab 1.2))
        leftp (if (= facing -1)
                (add-v2 abbig (resize-v2 (rotate-90-cw ab) -1.0))
                (add-v2 abbig (resize-v2 (rotate-90-cw ab) 30.0)))
        rightp (if (= facing -1)
                (add-v2 abbig (resize-v2 (rotate-90-ccw ab) 30.0))
                (add-v2 abbig (resize-v2 (rotate-90-ccw ab) -1.0)))
        topp (if (= facing -1)
                (add-v2 leftp (resize-v2 ab -15.0))
                (add-v2 rightp (resize-v2 ab -15.0)))]
    [topp leftp rightp]))


(defn gen-head-triangles [pa pb facing stroke]
  (let [ab (sub-v2 pb pa)
        ba (sub-v2 pa pb)
        npa (add-v2 pa (resize-v2 ba stroke))
        nlsa (add-v2 npa (resize-v2 (rotate-90-ccw ab) (+ 10.0 stroke)))
        nrsa (add-v2 npa (resize-v2 (rotate-90-cw ab) (+ 10.0 stroke)))
        nlea (add-v2 pb (resize-v2 (rotate-90-ccw ab) (+ 11.0 stroke)))
        nrea (add-v2 pb (resize-v2 (rotate-90-cw ab) (+ 11.0 stroke)))
        ab23 (add-v2 npa (scale-v2 ab 0.66))
        nose (if (= -1 facing)
               (add-v2 ab23 (resize-v2 (rotate-90-ccw ab) (+ 15.0 stroke)))
               (add-v2 ab23 (resize-v2 (rotate-90-cw ab) (+ 15.0 stroke))))]
    (if (= -1 facing)
      [nlsa nrsa nrea nlsa nrea nose nose nlea nrea]
      [nlsa nrsa nose nlsa nose nlea nose nrea nlea])))


(defn get-skin-triangles
  [{{:keys [head neck hip elbow_l elbow_r hand_l hand_r knee_l knee_r foot_l foot_r]} :masses
    {:keys [headw neckw armw hipw legw cola colb colc cold]} :metrics
    {af :active pf :passive} :base-order
    {as :active ps :passive} :base-surfaces
    facing :facing
    randoms :randoms}
   variation]
  (let [[r0 r1 r2 r3 r4 r5 r6 r7 r8 r9] (subvec randoms (* variation 10))] ; I just love clojure because of this!
    (concat []
            ;; legs
            (map #(concat % [0.0 0.0 0.0 1.0]) (gen-tube-triangles [(:p neck) (:p hip) (:p knee_r) (:p foot_r)] [5.0 (+ hipw 5 r0) (+ legw 5 r1) (+ legw 5 r2)])) ; stroke
            (map #(concat % colb) (gen-tube-triangles [(:p neck) (:p hip) (:p knee_r) (:p foot_r)]  [1.0 (+ hipw r3) (+ legw r4) (+ legw r5)]))
            ; feet
            (if (and (= pf :base_r) (not= ps nil))
              (map #(concat % colb)
                   (gen-foot-triangles (add-v2 (:p foot_r) (rotate-90-cw (:b ps)))
                                       (:p foot_r)
                                       (+ r6 5.0)
                                       facing))
              (map #(concat % colb)
                   (gen-foot-triangles (:p knee_r) (:p foot_r) (+ r6 5.0) facing)))
            
            (map #(concat % [0.0 0.0 0.0 1.0]) (gen-tube-triangles [(:p neck) (:p hip) (:p knee_l) (:p foot_l)] [6.0 (+ hipw 5 r7 ) (+ legw 5 r8) (+ legw 5 r9)])) ; stroke
            (map #(concat % cola) (gen-tube-triangles [(:p neck) (:p hip) (:p knee_l) (:p foot_l)] [1.0 (+ hipw r0) (+ legw r1) (+ legw r2)]))
            ;; feet
            (if (and (= pf :base_l) (not= ps nil))
              (map #(concat % cola)
                   (gen-foot-triangles (add-v2 (:p foot_l) (rotate-90-cw (:b ps)))
                                       (:p foot_l)
                                       (+ r3 5.0)
                                       facing))
              (map #(concat % cola)
                   (gen-foot-triangles (:p knee_l) (:p foot_l) (+ r3 5.0) facing)))
            
            (map #(concat % [0.0 0.0 0.0 1.0]) (gen-tube-triangles [(:p neck) (:p elbow_l) (:p hand_l)] [(+ armw 5.0 r4) (+ armw 5.0 r5) (+ armw 5.0 r6)])) ; stroke
            (map #(concat % colb) (gen-tube-triangles [(:p neck) (:p elbow_l) (:p hand_l)] [(+ armw r7) (+ armw r8) (+ armw r9)]))

            ;; body
            (map #(concat % [0.0 0.0 0.0 1.0]) (gen-tube-triangles [(:p head) (:p neck) (:p hip)] [(+ neckw 5.0 r0) (+ neckw 5.0 r1) (+ hipw 5.0 r2)]))
            (map #(concat % colc) (gen-tube-triangles [(:p head) (:p neck) (:p hip)] [(+ neckw r3) (+ neckw r4) (+ hipw r5)]))
            
            ;; head
            (map #(concat % [0.0 0.0 0.0 1.0]) (gen-head-triangles (:p head) (:p neck) facing (+ 5.0 r6)))
            (map #(concat % [0.8 0.5 0.5 1.0]) (gen-head-triangles (:p head) (:p neck) facing r7))
            
            ;; arms
            (map #(concat % [0.0 0.0 0.0 1.0]) (gen-tube-triangles [(:p neck) (:p elbow_r) (:p hand_r)] [(+ armw 5.0 r8) (+ armw 5.0 r9) (+ armw 5.0 r0)])) ; stroke
            (map #(concat % cola) (gen-tube-triangles [(:p neck) (:p elbow_r) (:p hand_r)] [(+ armw r1) (+ armw r2) (+ armw r3)])))))
  
