(ns brawl.shape
  (:require [clojure.set]))


(defn triangulate_area [points]
  (let [n (count points)]
    (loop [A 0.0
           p (- n 1 )
           q 0]
      (if (< q n )
        (let [[px py] (nth points p)
              [qx qy] (nth points q)]
        (recur (+ A ( - ( * px qy ) ( * qx py ) ) ) q ( + q 1 ) )
        )
        (* A 0.5 )
        )
      )  
    )
  )

(defn triangulate_inside_triangle [ Ax Ay Bx By Cx Cy Px Py ]
  (let [ax (- Cx Bx )
        ay (- Cy By )
        bx (- Ax Cx )
        by (- Ay Cy )
        cx (- Bx Ax )
        cy (- By Ay )
        apx ( - Px Ax )
        apy ( - Py Ay )
        bpx ( - Px Bx )
        bpy ( - Py By )
        cpx ( - Px Cx )
        cpy ( - Py Cy )
        aCrossbp ( - ( * ax bpy ) ( * ay bpx ) )
        cCrossap ( - ( * cx apy ) ( * cy apx ) )
        bCrosscp ( - ( * bx cpy ) ( * by cpx ) ) ]
     (and (> aCrossbp 0.0 ) ( > bCrosscp 0.0 ) ( > cCrossap 0.0 ) )
     )
  )

(def EPSILON 0.0000000001)


(defn triangulate_snip [ points u v w n V ]
  ( let [[Ax Ay] (nth points (nth V u) )
         [Bx By] (nth points (nth V v) )
         [Cx Cy] (nth points (nth V w) )
         e (- ( * (- Bx Ax ) (- Cy Ay ) ) ( * ( - By Ay ) ( - Cx Ax ) ) )]

   (if ( < EPSILON e )
     (loop [ p 0 ]
       (if (< p n )
         (do
           (let [ [Px Py] (nth points (nth V p) ) ]
             ( if (and (not= p u ) (not= p v ) ( not= p w ) )
              (do
                ( if ( triangulate_inside_triangle Ax Ay Bx By Cx Cy Px Py )
                 false
                 (recur (inc p)))
                )
              (recur (inc p) )
              )
             )
           )
         true
         )
       )
     false
     )
   )
  )

(defn triangulate_c [points]
  (let [length (count points)]
    (when (> length 3)
      (loop [;; actual indexes, we want a counter-clockwise polygon
             indexes (if (> (triangulate_area points) 0.0)
                       (vec (range 0 length))
                       (vec (reverse (range 0 length))) )
             ;; remaining points length
             remaining length
             ;; error detection counter
             ecounter (dec (* 2 remaining))
             ;; actual index in indexes
             actual (- remaining 1)
             ;; result container
             result []]
        (println "indexes" indexes)
        (if (> remaining 2)
          (do
            ;; select three consecutive vertices in polygon
            (let [va ( if (<= remaining actual) 0 actual )
                  vb ( if (<= remaining (inc va)) 0 (inc va) )
                  vc ( if (<= remaining (inc vb)) 0 (inc vb))                  
                  snipable ( triangulate_snip points va vb vc remaining indexes )]
              ;; if we arexs looping polygon is irregular
              (if (> ecounter -1 )
                ;; if snip is possible
                (if snipable
                  (let [pa (nth indexes va)
                        pb (nth indexes vb)
                        pc (nth indexes vc)]
                    (recur
                     ;; cut used indexes out
                     (vec
                      (concat
                       (subvec indexes 0 vb )
                       (subvec indexes (+ vb 1 ) (+ vb (- remaining vb ) ) )
                       (subvec indexes (+ vb (- remaining vb 1) ) ) ) )
                     (dec remaining)
                     (dec (* 2 (dec remaining)))
                     vb
                     (conj result (nth points pa) (nth points pb ) (nth points pc ))))
                  (recur indexes remaining (dec ecounter) vb result ))
                result)))
          result)))))



(triangulate_c[ '( 0.0 0.0 ) '( -5.0 5.0 ) '( 0.0 10.0 ) '( 10.0 10.0 ) '( 10.0 0.0 ) '( 5.0 -5.0 ) ] )

(triangulate_c
[[563.576 90.063] [566.246 83.125] [565.647 73.128] [559.187 61.8] [568.488 71.264] [570.83 82.929] [567.94 88.843] [563.57 90.063]])
