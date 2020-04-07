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
                (add-v2 abbig (resize-v2 (rotate-90-cw ab) 20.0)))
        rightp (if (= facing -1)
                (add-v2 abbig (resize-v2 (rotate-90-ccw ab) 20.0))
                (add-v2 abbig (resize-v2 (rotate-90-ccw ab) -1.0)))
        topp (if (= facing -1)
                (add-v2 leftp (resize-v2 ab -10.0))
                (add-v2 rightp (resize-v2 ab -10.0)))]
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
    {:keys [headw neckw bodyw hipw legw cola colb colc cold]} :metrics
    {af :active pf :passive} :foot-order
    {as :active ps :passive} :foot-surfaces
    facing :facing}]
  (concat []
          ;; feet
          (if (and (= pf :foot_l) (not= ps nil))
            (map #(concat % [0.5 0.0 0.5 1.0])
                 (gen-foot-triangles (add-v2 (:p foot_l) (rotate-90-cw (:b ps)))
                                     (:p foot_l)
                                     5.0
                                     facing))
            (map #(concat % [0.5 0.0 0.5 1.0])
                 (gen-foot-triangles (:p knee_l) (:p foot_l) 5.0 facing)))

          (if (and (= pf :foot_r) (not= ps nil))
            (map #(concat % [0.0 0.0 0.5 1.0])
                 (gen-foot-triangles (add-v2 (:p foot_r) (rotate-90-cw (:b ps)))
                                     (:p foot_r)
                                     5.0
                                     facing))
            (map #(concat % [0.0 0.0 0.5 1.0])
                 (gen-foot-triangles (:p knee_r) (:p foot_r) 5.0 facing)))
          ;; legs
          (map #(concat % [0.0 0.0 0.0 1.0]) (gen-tube-triangles [(:p neck) (:p hip) (:p knee_r) (:p foot_r)] [9.0 (+ hipw 3.0) (+ legw 3.0) (+ legw 3.0)])) ; stroke
          (map #(concat % colb) (gen-tube-triangles [(:p neck) (:p hip) (:p knee_r) (:p foot_r)] [6.0 hipw legw legw]))

          (map #(concat % [0.0 0.0 0.0 1.0]) (gen-tube-triangles [(:p neck) (:p hip) (:p knee_l) (:p foot_l)] [5.0 (+ hipw 3.0) (+ legw 3.0) (+ legw 3.0)])) ; stroke
          (map #(concat % cola) (gen-tube-triangles [(:p neck) (:p hip) (:p knee_l) (:p foot_l)] [1.0 hipw legw legw]))

          (map #(concat % [0.0 0.0 0.0 1.0]) (gen-tube-triangles [(:p neck) (:p elbow_l) (:p hand_l)] [8.0 8.0 8.0])) ; stroke
          (map #(concat % cola) (gen-tube-triangles [(:p neck) (:p elbow_l) (:p hand_l)] [5.0 5.0 5.0]))

          ;; body
          (map #(concat % colc) (gen-tube-triangles [(:p head) (:p neck) (:p hip)] [neckw neckw hipw]))

          ;; head
          (map #(concat % [0.0 0.0 0.0 1.0]) (gen-head-triangles (:p head) (:p neck) facing 3.0))
          (map #(concat % [0.8 0.5 0.5 1.0]) (gen-head-triangles (:p head) (:p neck) facing 1.0))

          ;; arms
          (map #(concat % [0.0 0.0 0.0 1.0]) (gen-tube-triangles [(:p neck) (:p elbow_r) (:p hand_r)] [8.0 8.0 8.0])) ; stroke
          (map #(concat % colb) (gen-tube-triangles [(:p neck) (:p elbow_r) (:p hand_r)] [5.0 5.0 5.0]))))
