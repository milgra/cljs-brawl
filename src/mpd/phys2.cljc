(ns mpd.phys2
  (:require [mpd.math2 :as math2]))


(defn mass2 [x y radius weight friction elasticity]
  "create basic structure"
  {:p [x y] ;; position
   :d [0 0] ;; direction
   :w weight
   :r radius
   :f friction
   :e elasticity
   :q false}) ;; quiescence


(defn dguard2 [masses massa massb distance elasticity]
  "create distance guard"
  (let [{wa :w} (masses massa)
        {wb :w} (masses massb)
        sum (+ wa wb)]
    {:a massa
     :b massb
     :d distance
     :e elasticity
     :ra (/ wa sum) ;; weight ratio of mass a
     :rb (/ wb sum) ;; weight ratio of mass b
     }))


(defn aguard2 [masses massa massb massc minangle maxangle power]
  "create angle guard"
  (let [{wa :w} (masses massa)
        {wc :w} (masses massc)
        sum (+ wa wc)]
    {:a massa
     :b massb
     :c massc
     :p power
     :ra (/ wa sum)
     :rb (/ wc sum)
     :min minangle
     :max maxangle}))


(defn segment2
  "creates segment 2d structure as [trans basis]"
  [[x y :as a][z v :as b]]
  ;; segment should be oriented from left to right
  {:t (if (< x z) a b)
   :b (if (< x z) (math2/sub-v2 b a) (math2/sub-v2 a b))})


(defn timescale [masses delta]
  "scale with timestepping"
  (reduce
   (fn [ result [mid { dir :d :as mass }]]
        (assoc result mid (assoc mass :d (math2/scale-v2 dir delta))))
   masses
   masses))


(defn surfaces-from-pointlist
  "Generates physics/segment2-s from surface point list array"
  [surfacepoints]
  (reduce
   (fn [result points]
     (concat
      result
      (reduce
       (fn [res [x y]] (conj res (segment2 x y)))
       []
       (partition 2 1 points))))
   []
   surfacepoints))


(defn get-colliding-surfaces [p d radius surfaces]
  "collect surfaces crossed by masspoint dir or nearby endpoint"
  (reduce
   (fn [result {t :t b :b :as surface}]
     (let [isp (math2/collide-v2-v2 p d t b radius)]
       (if-not (= isp nil)
         (let [dst (math2/length-v2 (math2/sub-v2 isp p))]
           (conj result [dst isp surface]))
         result)))
   []
   surfaces))


(defn get-colliding-surfaces-by-distance [p d radius surfaces point]
  "collect surfaces crossed by masspoint dir or nearby endpoint"
  (reduce
   (fn [result {t :t b :b :as surface}]
     (let [isp (math2/collide-v2-v2 p d t b radius)]
       (if-not (= isp nil)
         (let [dst (math2/length-v2 (math2/sub-v2 isp point))]
           (conj result [dst isp surface]))
         result)))
   []
   surfaces))


(defn get-intersecting-surfaces [p d surfaces]
  "collect surfaces crossed by masspoint dir or nearby endpoint"
  (reduce
   (fn [result {t :t b :b :as surface}]
     (let [isp (math2/intersect-v2-v2 p d t b)]
       (if-not (= isp nil)
         (let [dst (math2/length-v2 (math2/sub-v2 isp p))]
           (conj result [dst isp surface]))
         result)))
   []
   surfaces))


(defn move-mass-back [surface-trans surface-dir vector-trans vector-dir radius]
  "moves mass back to the point where radius doesn't touch the line"
  (let [[a b] surface-trans
        [c d] surface-dir
        [e f] vector-trans
        [g h] vector-dir
        [cx cy] (math2/isp-l2-l2 [a b] [c d] [e f] [d (- c)]) ; get intersection point
        [dx dy] [(- e cx)(- f cy)] ; vector from isp to vector trans
        [nx ny] (math2/resize-v2 [dx dy] radius)
        [fx fy] (math2/add-v2 [cx cy] [nx ny])] ; move back isp towards vector trans with radius
    (math2/isp-l2-l2 [fx fy] [c d] [e f] [g h])))


(defn keep-distances [masses dguards]
  (reduce
   (fn [result dguard]
     (let [{:keys [a b d e ra rb]} dguard
           {ta :p ba :d :as massa} (get result a)
           {tb :p bb :d :as massb} (get result b)
           fa (math2/add-v2 ta ba)
           fb (math2/add-v2 tb bb)
           conn (math2/sub-v2 fa fb)
           dist (- (math2/length-v2 conn) d)]
       (if (> (Math/abs dist) 0.01)
         (let [newdist (if (> e 0.0) (* dist e) dist)
               conna (math2/resize-v2 conn (- (* newdist ra)))
               connb (math2/resize-v2 conn (* newdist rb))
               newmassa (update massa :d math2/add-v2 conna)
               newmassb (update massb :d math2/add-v2 connb)]
           (-> result
               (assoc a newmassa)
               (assoc b newmassb)))
         result)))
       masses
       dguards))


(defn keep-angles
  "maintain min and max angles between two vectors"
  [masses aguards]
  (reduce
   (fn [result aguard]
     (let [{:keys [a b c p ra rc min max]} aguard
           {pa :p da :d :as massa} (get result a)
           {pb :p db :d :as massb} (get result b)
           {pc :p dc :d :as massc} (get result c)
           fa (math2/add-v2 pa da) ;; final a
           fb (math2/add-v2 pb db) ;; final b
           fc (math2/add-v2 pc dc) ;; final c
           fda (math2/sub-v2 fa fb)
           fdc (math2/sub-v2 fc fb)
           fdalength (math2/length-v2 fda)
           fdclength (math2/length-v2 fdc)
           angleda (math2/angle-x-v2 fda)
           angledc (math2/angle-x-v2 fdc)
           anglere (math2/normalize-angle (- angledc angleda))] ;; ccw angle difference 
       (if (or (< anglere min) (> anglere max))
         (let [diffmin (math2/normalize-angle (- min anglere)) ;; ccw delta
               diffmax (math2/normalize-angle (- anglere max)) ;; ccw delta
               ;; using smaller angle difference
               newangleda (if (< diffmin diffmax)
                            (- angleda ( * diffmin ra ) )
                            (+ angleda ( * diffmax ra ) ))
               newangledc (if (< diffmin diffmax)
                            (+ angledc ( * diffmin rc ) )
                            (- angledc ( * diffmax rc ) ))
               ;; calculate rotated da and dc
               ndax ( + (pb 0) (* (Math/cos newangleda) fdalength ))
               nday ( + (pb 1) (* (Math/sin newangleda) fdalength ))
               ndcx ( + (pb 0) (* (Math/cos newangledc) fdclength ))
               ndcy ( + (pb 1) (* (Math/sin newangledc) fdclength ))
               ;; calculate forces. b will move backwards because we rotate da and dc around their centers
               force_a (math2/scale-v2 (math2/sub-v2 [ndax nday] fa) (* 0.5 p))
               force_c (math2/scale-v2 (math2/sub-v2 [ndcx ndcy] fc) (* 0.5 p))
               force_b (math2/scale-v2 (math2/add-v2 force_a force_c) (* -0.5 p))
               ;; update dires
               newmassa (assoc massa :d ( math2/add-v2 da force_a))
               newmassb (assoc massb :d ( math2/add-v2 db force_b))
               newmassc (assoc massc :d ( math2/add-v2 dc force_c))]
           (-> result
               (assoc a newmassa)
               (assoc b newmassb)
               (assoc c newmassc)))
         result)))
   masses
   aguards))

;; in case of intersection with surface, move mass back to safe distance (radius from surface)
;; mirror dir on surface and reduce it with passed distance
;; if dir is smaller than radius stop movement

(defn move-mass
  "check collision of mass dir with surfaces, move mass to next iteration point and modify direction"
  [{:keys [p d r f e s] :as mass} surfaces gravity]
  (loop [prev-p p ;; previous position
         prev-d d ;; previous direction
         full-d d ;; final direction
         iter 0 ;; iteration
         done false
         quis false] ;; quisence
    (if done
      ;; no more iterations, assigning new point, direction, quisence
      (assoc mass :p prev-p :d full-d :q quis) ;; assoc last point and final dir          
      ;; new iterations, get colliding surfaces
      (let [results (sort-by first < (get-colliding-surfaces prev-p prev-d r surfaces))
            [dst isp {strans :t sbasis :b :as segment}] (first results)]
        (if segment
          ;; collision, calculate full and used size
          (let [normal-with-surface-component (math2/norm-p2-l2 (math2/add-v2 strans full-d) strans sbasis)
                parallel-with-surface-component (math2/norm-p2-l2 (math2/add-v2 strans full-d) strans (math2/rotate-90-cw sbasis))
                para-length (math2/length-v2 parallel-with-surface-component)
                norm-length (math2/length-v2 normal-with-surface-component)
                full-size (math2/length-v2 prev-d)
                used-size (- full-size dst)]
            (if (< norm-length (+ gravity 0.5))
              ;; normal component is smaller than gravity, start sliding
              (let [newfull-d (math2/scale-v2 parallel-with-surface-component (- 1.0 f))]
                ;;(println "norm-length" norm-length "gravity" gravity)
                ;;(println "coll surf" strans sbasis)
                ;;(println "coll point dir" prev-p prev-d)
                ;;(println "slide norm" normal-with-surface-component "para" parallel-with-surface-component "new" newfull-d "sbasis" sbasis)
                (recur isp
                       newfull-d
                       newfull-d
                       (inc iter)
                       false
                       true)) ;; send mass to next iteration, if it's not in a corner it will move without bounce/slide
              ;; full size is bigger than radius, bounce
              (let [newfull-d (math2/scale-v2 (math2/mirror-v2-bases sbasis full-d) e) ;; mirror full size of direction
                    new-d (math2/resize-v2 newfull-d used-size)]
                ;;(println "bounce")
                (recur isp
                       new-d
                       newfull-d
                       (inc iter)
                       (> iter 4)
                       false))))
          ;; move point to new position, finish
          (recur (math2/add-v2 prev-p prev-d)
                 prev-d
                 full-d
                 (inc iter)
                 true
                 quis))))))


(defn move-masses [masses surfaces gravity]
  "check collisions and move masses to new positions considering collisions"
  (reduce
   (fn [result [id mass]]
     (let [newmass (move-mass mass surfaces gravity)]
       (assoc result id newmass)))
   masses
   masses))


(defn add-gravity [masses gravity]
  "adds gravity vector to masspoints dires"
  (reduce
   (fn [result [mid mass]]
     (let [dir (:d mass)
           newmass (assoc mass :d (math2/add-v2 dir gravity))]
       (assoc result mid newmass)))
   masses
   masses))
