(ns brawl.actorskin
  (:require [brawl.floatbuffer :as fb])
  (:use [mpd.math2 :only [rotate-up-v2 resize-v2 scale-v2 rotate-90-cw rotate-90-ccw add-v2 sub-v2]]))

(def black [0.0 0.0 0.0 1.0])

(defn getpoints [{{{[e f] :p} :hip :as masses} :masses} floatbuffer [l r b t]]
  (if (and (< l e) (> r e) (< t f) (> b f)) 
    (reduce (fn [oldbuf [mid {[x y] :p :as mass}]] (fb/append! oldbuf (array x y 1.0 1.0 1.0 1.0))) floatbuffer masses)
    floatbuffer))


(defn getlines [{{:keys [head neck hip elbow_l elbow_r hand_l hand_r knee_l knee_r foot_l foot_r] {[e f] :p} :hip} :masses} floatbuffer [l r b t]]
  (if (and (< l e) (> r e) (< t f) (> b f)) 
    (let [[a b] (:p head)
          [c d] (:p neck)
          [g h] (:p knee_l)
          [i j] (:p foot_l)
          [k l] (:p knee_r)
          [m n] (:p foot_r)
          [o p] (:p elbow_l)
          [q r] (:p hand_l)
          [u v] (:p elbow_r)
          [x y] (:p hand_r)]
      (fb/append! floatbuffer (array
                  a b 1 1 1 1 c d 1 1 1 1 ;; head -> neck
                  c d 1 1 1 1 e f 1 1 1 1 ;; neck -> hip
                  e f 1 1 1 1 g h 1 1 1 1 ;; hip -> knee_l
                  g h 1 1 1 1 i j 1 1 1 1 ;; knee_l -> foot_l
                  e f 1 1 1 1 k l 1 1 1 1 ;; hip -> knee_r
                  k l 1 1 1 1 m n 1 1 1 1 ;; knee_r -> foot_r
                  c d 1 1 1 1 o p 1 1 1 1 ;; neck -> elbow_l
                  o p 1 1 1 1 q r 1 1 1 1 ;; elbow_l -> hand_l
                  c d 1 1 1 1 u v 1 1 1 1 ;; neck -> elbow_r
                  u v 1 1 1 1 x y 1 1 1 1 ;; elbow_r -> hand_r
                  )))
    floatbuffer))

(defn gen-tube-triangles [buf points sizes [x y z w]]
  (loop [rempts points
         remszs sizes
         oldbuf buf]
    (if (= 2 (count rempts))
      ;; close tube
      (let [pa (nth rempts 0)
            pb (nth rempts 1)
            sa (nth remszs 0)
            sb (nth remszs 1)
            ab (sub-v2 pb pa)
            [a b :as nlsa] (add-v2 pa (resize-v2( rotate-90-ccw ab) sa))
            [c d :as nrsa] (add-v2 pa (resize-v2( rotate-90-cw ab) sa))
            [e f :as nlea] (add-v2 pb (resize-v2( rotate-90-ccw ab) sb))
            [g h :as nrea] (add-v2 pb (resize-v2( rotate-90-cw ab) sb))]
        ;; nlsa nrsa nrea
        ;; nlsa nrea nlea
        (fb/append! oldbuf (array a b x y z w c d x y z w g h x y z w a b x y z w g h x y z w e f x y z w)))
      ;; add rect and joint triangle
      (let [pa (nth rempts 0)
            [m n :as pb] (nth rempts 1)
            pc (nth rempts 2)
            sa (nth remszs 0)
            sb (nth remszs 1)
            ab (sub-v2 pb pa)
            bc (sub-v2 pc pb)
            [a b :as nlsa] (add-v2 pa (resize-v2( rotate-90-ccw ab) sa))
            [c d :as nrsa] (add-v2 pa (resize-v2( rotate-90-cw ab) sa))
            [e f :as nlea] (add-v2 pb (resize-v2( rotate-90-ccw ab) sb))
            [g h :as nrea] (add-v2 pb (resize-v2( rotate-90-cw ab) sb))
            [i j :as nlsb] (add-v2 pb (resize-v2( rotate-90-ccw bc) sb))
            [k l :as nrsb] (add-v2 pb (resize-v2( rotate-90-cw bc) sb))]
        ;; nlsa nrsa nrea
        ;; nlsa nrea nlea
        ;; nlea nlsb pb
        ;; nrea nrsb pb
        (recur (rest rempts)
               (rest remszs)
               (fb/append! oldbuf (array a b x y z w c d x y z w g h x y z w a b x y z w g h x y z w e f x y z w
                                         e f x y z w i j x y z w m n x y z w g h x y z w k l x y z w m n x y z w)))

               ))))


(defn gen-foot-triangles [buf pa pb size facing [x y z w]]
  (let [ab (resize-v2 (sub-v2 pb pa) (+ 20.0 size))
        npb (add-v2 pb (resize-v2 ab size))
        [a b :as leftp] (if (= facing -1)
                (add-v2 npb (resize-v2 (rotate-90-cw ab) (+ 2.0 size)))
                (add-v2 pb (resize-v2 (rotate-90-cw ab) 30.0)))
        [c d :as rightp] (if (= facing -1)
                (add-v2 pb (resize-v2 (rotate-90-ccw ab) 30.0))
                (add-v2 npb (resize-v2 (rotate-90-ccw ab) (+ 2.0 size))))
        [e f :as topp] (if (= facing -1)
                (add-v2 leftp (resize-v2 ab (+ -15.0 size)))
                (add-v2 rightp (resize-v2 ab (+ -15.0 size))))]
    (fb/append! buf (array e f x y z w a b x y z w c d x y z w))))


(defn gen-head-triangles [buf pa pb facing stroke [x y z w]]
  (let [ab (sub-v2 pb pa)
        ba (sub-v2 pa pb)
        npa (add-v2 pa (resize-v2 ba stroke))
        [a b :as nlsa] (add-v2 npa (resize-v2 (rotate-90-ccw ab) (+ 10.0 stroke)))
        [c d :as nrsa] (add-v2 npa (resize-v2 (rotate-90-cw ab) (+ 10.0 stroke)))
        [e f :as nlea] (add-v2 pb (resize-v2 (rotate-90-ccw ab) (+ 11.0 stroke)))
        [g h :as nrea] (add-v2 pb (resize-v2 (rotate-90-cw ab) (+ 11.0 stroke)))
        ab23 (add-v2 npa (scale-v2 ab 0.66))
        [i j :as nose] (if (= -1 facing)
               (add-v2 ab23 (resize-v2 (rotate-90-ccw ab) (+ 15.0 stroke)))
               (add-v2 ab23 (resize-v2 (rotate-90-cw ab) (+ 15.0 stroke))))]
    (if (= -1 facing)
      (fb/append! buf (array a b x y z w c d x y z w g h x y z w a b x y z w g h x y z w i j x y z w i j x y z w e f x y z w g h x y z w))
      (fb/append! buf (array a b x y z w c d x y z w i j x y z w a b x y z w i j x y z w e f x y z w i j x y z w g h x y z w e f x y z w))
      ;;[nlsa nrsa nrea nlsa nrea nose nose nlea nrea]
      ;;[nlsa nrsa nose nlsa nose nlea nose nrea nlea]
      )))


(defn get-skin-triangles
  [{{:keys [head neck elbow_l elbow_r hand_l hand_r knee_l knee_r foot_l foot_r] {[x y] :p :as hip} :hip} :masses
    {:keys [headw neckw armw hipw legw cola colb colc cold]} :metrics
    {af :active pf :passive} :base-order
    {as :active ps :passive} :base-surfaces
    facing :facing
    randoms :randoms
    health :health
    color :colorf
    }
   floatbuffer
   variation
   [l r b t]]
  (if (and (< l x) (> r x) (< t y) (> b y)) 
    (let [[r0 r1 r2 r3 r4 r5 r6 r7 r8 r9] (subvec randoms (* variation 10))] ; I just love clojure because of this!
      (cond-> floatbuffer

        ;; right legs
        true (gen-tube-triangles [(:p neck) (:p hip) (:p knee_r) (:p foot_r)] [5.0 (+ hipw 5 r0) (+ legw 5 r1) (+ legw 5 r2)] black) ; stroke
        true (gen-tube-triangles [(:p neck) (:p hip) (:p knee_r) (:p foot_r)] [1.0 (+ hipw r3) (+ legw r4) (+ legw r5)] colb)
        
        ;; passive right
        (and (= pf :base_r) (not= ps nil)) (gen-foot-triangles (add-v2 (:p foot_r) (rotate-up-v2 (:b ps))) (:p foot_r) (+ r6 5.0) facing black)
        (and (= pf :base_r) (not= ps nil)) (gen-foot-triangles (add-v2 (:p foot_r) (rotate-up-v2 (:b ps))) (:p foot_r) r6 facing colb)

        ;; active right
        (and (= af :base_r) (not= ps nil)) (gen-foot-triangles (:p knee_r) (:p foot_r) (+ r6 5.0) facing black)
        (and (= af :base_r) (not= ps nil)) (gen-foot-triangles (:p knee_r) (:p foot_r) r6 facing colb)

        true (gen-tube-triangles [(:p neck) (:p hip) (:p knee_l) (:p foot_l)] [6.0 (+ hipw 5 r7 ) (+ legw 5 r8) (+ legw 5 r9)] black) ; stroke
        true (gen-tube-triangles [(:p neck) (:p hip) (:p knee_l) (:p foot_l)] [1.0 (+ hipw r0) (+ legw r1) (+ legw r2)] cola)

        ;; passive left
        (and (= pf :base_l) (not= ps nil)) (gen-foot-triangles (add-v2 (:p foot_l) (rotate-up-v2 (:b ps))) (:p foot_l) (+ r3 5.0) facing black)
        (and (= pf :base_l) (not= ps nil)) (gen-foot-triangles (add-v2 (:p foot_l) (rotate-up-v2 (:b ps))) (:p foot_l) r3 facing cola)

        ;; active left
        (and (= af :base_l) (not= ps nil)) (gen-foot-triangles (:p knee_l) (:p foot_l) (+ r3 5.0) facing black)
        (and (= af :base_l) (not= ps nil)) (gen-foot-triangles (:p knee_l) (:p foot_l) r3 facing cola)
        
        true (gen-tube-triangles [(:p neck) (:p elbow_l) (:p hand_l)] [(+ armw 5.0 r4) (+ armw 5.0 r5) (+ armw 5.0 r6)] black) ; stroke
        true (gen-tube-triangles [(:p neck) (:p elbow_l) (:p hand_l)] [(+ armw r7) (+ armw r8) (+ armw r9)] colb)
        
        ;; body
        true (gen-tube-triangles [(:p head) (:p neck) (:p hip)] [(+ neckw 5.0 r0) (+ neckw 5.0 r1) (+ hipw 5.0 r2)] black)
        true (gen-tube-triangles [(:p head) (:p neck) (:p hip)] [(+ neckw r3) (+ neckw r4) (+ hipw r5)] color)
        
        ;; head
        true (gen-head-triangles (:p head) (:p neck) facing (+ 5.0 r6) [(/ (- 100.0 health) 100.0) 0.0 0.0 1.0])
        true (gen-head-triangles (:p head) (:p neck) facing r7 [0.8 0.5 0.5 1.0])
        
        ;; arms
        true (gen-tube-triangles [(:p neck) (:p elbow_r) (:p hand_r)] [(+ armw 5.0 r8) (+ armw 5.0 r9) (+ armw 5.0 r0)] black) ; stroke
        true (gen-tube-triangles [(:p neck) (:p elbow_r) (:p hand_r)] [(+ armw r1) (+ armw r2) (+ armw r3)] cola)))
    floatbuffer))
