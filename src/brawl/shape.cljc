(ns brawl.shape)


(def EPSILON 0.0000000001)

;; I leave this here if the second function below causes problems later (slow, or removing the last item is needed)
;;(defn triangulate_area [points]
;;  "calculates area of polygon"  
;;  (loop [sum 0.40
;;         pa (- (count points) 1)
;;         pb 0]
;;    (if (< pb (count points) )
;;      (let [[px py] (nth points pa)
;;            [qx qy] (nth points pb)]
;;        (recur (+ sum (- (* px qy) (* qx py))) pb (+ pb 1)))
;;      (/ sum 2.0 ))))


(defn triangulate_area [points]
  "calculates area of polygon"
  (/ (reduce
   (fn [ sum [[ax ay] [bx by]] ]
     (+ sum (- (* ax by) (* bx ay))))
   0.0
   (partition 2 1 (concat [(last points)] points))) 2.0))


(defn triangulate_inside_triangle [ Ax Ay Bx By Cx Cy Px Py ]
  "checks if point is inside triangle"
  (let [ax (- Cx Bx)
        ay (- Cy By)
        bx (- Ax Cx)
        by (- Ay Cy)
        cx (- Bx Ax)
        cy (- By Ay)
        apx (- Px Ax)
        apy (- Py Ay)
        bpx (- Px Bx)
        bpy (- Py By)
        cpx (- Px Cx)
        cpy (- Py Cy)
        aCrossbp (- (* ax bpy) (* ay bpx))
        cCrossap (- (* cx apy) (* cy apx))
        bCrosscp (- (* bx cpy) (* by cpx))]
     (and (> aCrossbp 0.0) (> bCrosscp 0.0) (> cCrossap 0.0))))


(defn triangulate_snips? [ points indexes va vb vc ]
  "checks if vertexes can be snipped out"
  ( let [[Ax Ay] (nth points (nth indexes va) )
         [Bx By] (nth points (nth indexes vb) )
         [Cx Cy] (nth points (nth indexes vc) )
         area (- (* (- Bx Ax ) (- Cy Ay)) (* (- By Ay) (- Cx Ax)))]
   (if (< EPSILON area)
     (loop [actual 0]
       (if (< actual (count indexes) )
         (do
           (let [[Px Py] (nth points (nth indexes actual))]
             (if (and (not= actual va) (not= actual vb) (not= actual vc))
              (if ( triangulate_inside_triangle Ax Ay Bx By Cx Cy Px Py)
                false
                (recur (inc actual)))
              (recur (inc actual)))))
         ;; if we went through all points and wasn't inside we are snippable
         true))
     ;;if area is negative or small no snippable
     false)))


(defn triangulate_c [points]
  "creates ccw triangles that makes up the polygon"
  (let [length (count points)]
    (when (> length 3)
      (loop [;; actual indexes, we want a counter-clockwise polygon
             indexes (if (> (triangulate_area points) 0.0)
                       (vec (range 0 length))
                       (vec (reverse (range 0 length))) )
             ;; error detection counter
             ecounter (dec (* 2 length ) )
             ;; actual index in indexes, starting from the end
             actual (- length 1)
             ;; result container
             result []]
        (if (> (count indexes ) 2)
          (do
            ;; select three consecutive vertices in polygon
            (let [remaining (count indexes)
                  va ( if (<= remaining actual) 0 actual)
                  vb ( if (<= remaining (inc va)) 0 (inc va))
                  vc ( if (<= remaining (inc vb)) 0 (inc vb))]
              ;; if we are looping polygon is irregular
              (if (> ecounter -1 )
                (if (triangulate_snips? points indexes va vb vc)
                  ;; if snip is possible
                  (let [pa (nth indexes va)
                        pb (nth indexes vb)
                        pc (nth indexes vc)]
                    (recur
                     ;; cut used indexes out
                     (vec
                      (concat
                       (subvec indexes 0 vb)
                       (subvec indexes (+ vb 1) (+ vb (- remaining vb)))))
                     (dec (* 2 (dec remaining)))
                     vb
                     (conj result
                           (nth points pa)
                           (nth points pb)
                           (nth points pc))))
                  ;; if snip not possible
                  (recur indexes (dec ecounter) vb result))
                result)))
          result)))))


;;(triangulate_area[ '( 0.0 0.0 ) '( -5.0 5.0 ) '( 0.0 10.0 ) '( 10.0 10.0 ) '( 10.0 0.0 ) '( 5.0 -5.0 ) ] )
;;(triangulate_c [[563.576 90.063] [566.246 83.125] [565.647 73.128] [559.187 61.8] [568.488 71.264] [570.83 82.929] [567.94 88.843] [563.57 90.063]])
