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
  (let [n (count points)]
    (when (> n 3)
      (let [area (triangulate_area points)
            ;; we want a counter-clockwise polygon
            V (if (> area 0.0)
                     (vec (range 0 n))
                     (vec (reverse (range 0 n))) )]
        (loop [m 0
               nv n
               v (- nv 1)
               ;; remove nv-2 verices, creating 1 triangle every time
               count (dec (* 2 nv))
               result []
               NV V]
          (if (> nv 2 )
            (do
              (let [tu v
                    _u ( if ( <= nv tu ) 0 tu )
                    tv (+ _u 1 )
                    _v ( if ( <= nv tv ) 0 tv )
                    tw (+ _v 1 )
                    _w ( if ( <= nv tw ) 0 tw )
                    tri ( triangulate_snip points _u _v _w nv NV )]
                (if (> count -1 )
                  (if tri
                    ( let [a (nth NV _u)
                           b (nth NV _v)
                           c (nth NV _w)]
                     ( recur
                      (inc m)
                      (dec nv)
                      _v 
                      (dec (* 2 (dec nv)))
                      (conj result (nth points a) (nth points b ) (nth points c ) )
                      (vec (concat (subvec NV 0 _v ) (subvec NV (+ _v 1 ) (+ _v (- nv _v ) ) ) (subvec NV (+ _v (- nv _v 1) ) ) ) )
                      )
                     )
                    (recur m nv _v (dec count) result NV)
                    )
                  result
                  )
                )
              )
            result
            )
          )
        )
      )    
    )
  )


(triangulate_c[ '( 0.0 0.0 ) '( -5.0 5.0 ) '( 0.0 10.0 ) '( 10.0 10.0 ) '( 10.0 0.0 ) '( 5.0 -5.0 ) ] )

(triangulate_c
[[563.576 90.063] [566.246 83.125] [565.647 73.128] [559.187 61.8] [568.488 71.264] [570.83 82.929] [567.94 88.843] [563.57 90.063]])
