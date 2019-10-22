(ns brawl.shape
  (:require [clojure.set]))

(def abs #?(:clj Math/abs :cljs js/Math.abs))
(def pow #?(:clj Math/pow :cljs js/Math.pow))


;; https://gist.github.com/mutoo/5617691
(defn circumscribe-triangle [[[ax ay] [bx by] [cx cy]]]
  (let [A (- bx ax)
        B (- by ay)
        C (- cx ax)
        D (- cy ay)
        E (+ (* A (+ ax bx)) (* B (+ ay by)))
        F (+ (* C (+ ax cx)) (* D (+ ay cy)))
        G (* 2 (- (* A (- cy by)) (* B (- cx bx))))]
    (when (> (abs G) 0.000001)
      (let [cx (/ (- (* D E) (* B F)) G)
            cy (/ (- (* A F) (* C E)) G)
            dx (- cx ax)
            dy (- cy ay)
            r  (+ (pow dx 2) (pow dy 2))]
        {:x cx :y cy :radius-squared r}))))

(defn edges [[p1 p2 p3]] [[p1 p2] [p2 p3] [p3 p1]])

(defn contains-point? [{:keys [x y radius-squared]} [px py]]
  (let [distance-squared (+ (pow (- x px) 2) (pow (- y py) 2))]
    (< distance-squared radius-squared)))

(defn outer-edges [triangles]
  (let [all-edges    (mapcat edges triangles)
        matches      (fn [edge] (filter #{edge (reverse edge)} all-edges))
        appears-once (fn [edge] (= (count (matches edge)) 1))]
    (filter appears-once all-edges)))

(defn make-new-triangles [containers point]
  (->> containers
       outer-edges
       (map (fn [[p1 p2]] [p1 p2 point]))
       set))

(defn add-point-to-triangles [triangles point]
  (let [containers    (filter #(contains-point? (circumscribe-triangle %) point) triangles)
        new-triangles (make-new-triangles containers point)]
    (clojure.set/union (clojure.set/difference triangles containers) new-triangles)))

(defn bounds [points]
  (let [minx (->> points (map first) (apply min) (+ -1000))
        maxx (->> points (map first) (apply max) (+ 1000))
        miny (->> points (map second) (apply min) (+ -1000))
        maxy (->> points (map second) (apply max) (+ 1000))]
    [[minx maxy] [maxx maxy] [minx miny] [maxx miny]]))

;; http://paulbourke.net/papers/triangulate/
(defn triangulate [points]
  (let [points (map (fn [[x y]] [(float x) (float y)]) points)
        [tl tr bl br] (bounds points)
        initial #{[tl tr bl] [bl tr br]}
        with-bounds (reduce add-point-to-triangles initial points)
        triangles (remove #(some #{tl tr bl br} %) with-bounds)]
    {:points points
     :triangles triangles
     :edges (distinct (mapcat edges triangles))}))








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
 ;; (println "triangulate snip u" u "v" v "w" w "n" n "e" e)

   (if ( < EPSILON e )
     (loop [ p 0 ]
       (if (< p n )
         (do
          ;; (println "p" p "u" u "v" v "w" w )

           (let [ [Px Py] (nth points (nth V p) ) ]
             ( if (and (not= p u ) (not= p v ) ( not= p w ) )
              (do
                ;;(println "Px" Px "Py" Py )
                ( if ( triangulate_inside_triangle Ax Ay Bx By Cx Cy Px Py )
                 false)
                )
              )
             (recur (inc p) )
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
        ;;(println "area " area "V" V)
        (loop [m 0
               nv n
               v (- nv 1)
               ;; remove nv-2 verices, creating 1 triangle every time
               count (dec (* 2 nv))
               result []
               NV V]
          (if (> nv 2 )
            (do
              ;;(println "NV" NV )
              ;;(println "m" m "nv" nv "count" count "result" result)
   
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
                     ;;(println "a b c" a b c (type c) )
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


(triangulate_c[ [ 0.0 0.0 ] [ -5.0 5.0 ] [ 0.0 10.0 ] [ 10.0 10.0 ] [ 10.0 0.0 ] [ 5.0 -5.0 ] ] )
