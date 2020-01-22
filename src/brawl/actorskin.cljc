(ns brawl.actorskin
  (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))


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


(defn gen-tube-triangles [ points sizes]

  (loop [rempts points
         remszs sizes
         result []]
    (if (= 2 (count rempts))
      ;; close tube
      (let [pa (nth rempts 0)
            pb (nth rempts 1)
            sa (nth remszs 0)
            sb (nth remszs 1)
            ab (math2/sub-v2 pb pa)
            nlsa (math2/add-v2 pa (math2/resize-v2( math2/rotate-90-ccw ab) sa))
            nrsa (math2/add-v2 pa (math2/resize-v2( math2/rotate-90-cw ab) sa))
            nlea (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-ccw ab) sb))
            nrea (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-cw ab) sb))]
        (conj result
              nlsa nrsa nrea
              nlsa nrea nlea))
      ;; add rect and joint triangle
      (let [pa (nth rempts 0)
            pb (nth rempts 1)
            pc (nth rempts 2)
            sa (nth remszs 0)
            sb (nth remszs 1)
            ab (math2/sub-v2 pb pa)
            bc (math2/sub-v2 pc pb)
            nlsa (math2/add-v2 pa (math2/resize-v2( math2/rotate-90-ccw ab) sa))
            nrsa (math2/add-v2 pa (math2/resize-v2( math2/rotate-90-cw ab) sa))
            nlea (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-ccw ab) sb))
            nrea (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-cw ab) sb))
            nlsb (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-ccw bc) sb))
            nrsb (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-cw bc) sb))]
        (recur (rest rempts)
               (rest remszs)
               (conj result
                     nlsa nrsa nrea
                     nlsa nrea nlea
                     nlea nlsb pb
                     nrea nrsb pb))))))


(defn gen-foot-triangles [pa pb size facing]
  (let [ab (math2/resize-v2 (math2/sub-v2 pb pa) 20.0)
        abbig pb ;;(math2/add-v2 pb (math2/scale-v2 ab 1.2))
        leftp (if (= facing -1)
                (math2/add-v2 abbig (math2/resize-v2 (math2/rotate-90-cw ab) -1.0))
                (math2/add-v2 abbig (math2/resize-v2 (math2/rotate-90-cw ab) 20.0)))
        rightp (if (= facing -1)
                (math2/add-v2 abbig (math2/resize-v2 (math2/rotate-90-ccw ab) 20.0))
                (math2/add-v2 abbig (math2/resize-v2 (math2/rotate-90-ccw ab) -1.0)))
        topp (if (= facing -1)
                (math2/add-v2 leftp (math2/resize-v2 ab -10.0))
                (math2/add-v2 rightp (math2/resize-v2 ab -10.0)))]
    [topp leftp rightp]))


(defn gen-head-triangles [pa pb facing]
  (let [ab (math2/sub-v2 pb pa)
        nlsa (math2/add-v2 pa (math2/resize-v2( math2/rotate-90-ccw ab) 10.0))
        nrsa (math2/add-v2 pa (math2/resize-v2( math2/rotate-90-cw ab) 10.0))
        nlea (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-ccw ab) 11.0))
        nrea (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-cw ab) 11.0))
        ab23 (math2/add-v2 pa (math2/scale-v2 ab 0.66))
        nose (if (= -1 facing)
               (math2/add-v2 ab23 (math2/resize-v2 (math2/rotate-90-ccw ab) 15.0))
               (math2/add-v2 ab23 (math2/resize-v2 (math2/rotate-90-cw ab) 15.0)))]
    (if (= -1 facing)
      [nlsa nrsa nrea nlsa nrea nose nose nlea nrea]
      [nlsa nrsa nose nlsa nose nlea nose nrea nlea])))


(defn get-skin-triangles
  [{{:keys [head neck hip elbow_a elbow_b hand_a hand_b knee_a knee_b ankle_a ankle_b]} :masses
    {:keys [headw neckw bodyw hipw legw]} :metrics
    {:keys [activebase passivebase activesurf passivesurf]} :walk
    facing :facing}]

  (concat []
          ;; feet
          (if (and (= passivebase :base_a) (not= passivesurf nil))
            (gen-foot-triangles (math2/add-v2 (ankle_a :p) (math2/rotate-90-cw (passivesurf :b)))
                                (ankle_a :p)
                                5.0
                                facing) 
            (gen-foot-triangles (knee_a :p) (ankle_a :p) 5.0 facing))
          (if (and (= passivebase :base_b) (not= passivesurf nil))
            (gen-foot-triangles (math2/add-v2 (ankle_b :p) (math2/rotate-90-cw (passivesurf :b)))
                                (ankle_b :p)
                                5.0
                                facing)
            (gen-foot-triangles (knee_b :p) (ankle_b :p) 5.0 facing))
          ;; legs
          (gen-tube-triangles [(:p neck) (:p hip) (:p knee_a) (:p ankle_a)] [1.0 hipw legw legw])
          (gen-tube-triangles [(:p neck) (:p hip) (:p knee_b) (:p ankle_b)] [6.0 hipw legw legw])
          ;; arms
          (gen-tube-triangles [(:p neck) (:p elbow_a) (:p hand_a)] [5.0 5.0 5.0])
          (gen-tube-triangles [(:p neck) (:p elbow_b) (:p hand_b)] [5.0 5.0 5.0])
          ;; body
          (gen-tube-triangles [(:p head) (:p neck) (:p hip)] [neckw neckw hipw])
          ;; head
          (gen-head-triangles (:p head) (:p neck) facing)))
