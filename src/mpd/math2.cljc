(ns mpd.math2)


(defn add-v2 [[ax ay][bx by]]
  [(+ ax bx) (+ ay by)])


(defn sub-v2 [[ax ay][bx by]]
  [(- ax bx) (- ay by)])


(defn length-v2 [[ax ay]]
  (Math/sqrt (+ (* ax ax) (* ay ay))))


(defn scale-v2 [[x y] ratio]
  [(* x ratio) (* y ratio)])


(defn resize-v2 [[x y] size]
  (if (and (not= size 0) (or (not= x 0) (not= y 0)))
    (let [ratio (/ size (length-v2 [x y]))]
      [(* x ratio) (* y ratio)])
    [x y]))


(defn angle-x-v2 [[x y]]
  "angle of vector from x axis"
  (Math/atan2 y x))


(defn normalize-angle [angle]
  "bring angle between 0 and 2PI radians"
  (if (< angle 0)
    (+ angle Math/PI Math/PI)
    angle))


(defn isp-l2-l2 [[tax tay][bax bay][tbx tby][bbx bby]]
  "line-line intersection calculation based on determinant calculation : Ax + By = C
   parameters : trans a basis a trans b basis b"
  (if (or
       (and (= bax 0)(= bay 0))
       (and (= bbx 0)(= bby 0)))
    nil ;; invalid line
    (let [A1 bay
          B1 (- bax)
          C1 (+ (* A1 tax) (* B1 tay))
          A2 bby
          B2 (- bbx)
          C2 (+ (* A2 tbx) (* B2 tby))
          DT (- (* A2 B1) (* B2 A1))]
      (if (= DT 0)
       nil ;; parallel lines
       [( / (- (* B1 C2) (* B2 C1)) DT )
        ( / (- (* A2 C1) (* A1 C2)) DT) ]))))


(defn p2-in-v2? [[px py] [tx ty] [bx by] radius]
  "check if p is inside vector defined by trans and basis with given radius from endpoints
   it checks point distance from the halfpoint of the vector"
  (let [lx (/ bx 2)
        ly (/ by 2)
        cx (+ tx lx)
        cy (+ ty ly)
        dx (- px cx)
        dy (- py cy)
        xok (< (Math/abs dx) (+ (Math/abs lx) radius)) 
        yok (< (Math/abs dy) (+ (Math/abs ly) radius))]
    (and xok yok)))


(defn dist-p2-l2 [[px py] [tx ty] [bx by]]
  "calculate distance of point and line
   https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line #Line defined by two points"
  (let [cx (+ tx bx)
        cy (+ ty by)]
    (/
     (Math/abs (+ (* by px) (* bx py -1) (* cx ty) (* cy tx -1)))
     (Math/sqrt (+ (* bx bx) (* by by))))))


(defn norm-p2-l2 [[px py] [tx ty] [bx by]]
  (let [[cx cy] (isp-l2-l2 [tx ty] [bx by] [px py] [by (- bx)])]
    [(- px cx) (- py cy)]))


(defn intersect-v2-v2
  "intersection point of two vectors"
  [transa basisa transb basisb]
  (let [cp (isp-l2-l2 transa basisa transb basisb)]
    (if (= cp nil)
      nil
      (if (and
          (p2-in-v2? cp transa basisa 1.0) ; point in on first vector
          (p2-in-v2? cp transb basisb 1.0)) ; point is on translated second vector
       cp
       nil))))

(defn collide-v2-v2
  "gets the point wheren vector touches the other vector translated towards the first vector by radius"
  [[e f :as ta] [g h :as ba] [a b :as tb] [c d :as bb] radius]
  ; should fast check first with radiuses
  (if (or
       (and (= g 0)(= h 0))
       (and (= c 0)(= d 0)))
    nil ;; invalid line
    (let [[cx cy] (isp-l2-l2 tb bb ta [d (- c)]) ; get isp of surface and perpendicular of surface from trans
          [dx dy] [(- e cx)(- f cy)] ; vector from isp to vector trans
          [nx ny] (resize-v2 [dx dy] radius)
          [fx fy] (add-v2 tb [nx ny]) ; move tb towards vector trans with radius
          result (isp-l2-l2 [fx fy] bb ta ba)] ; isp of vector and translated surface
      (if (and
          (p2-in-v2? result ta ba 1.0) ; point is on first vector
          (p2-in-v2? result [fx fy] bb 1.0)) ; point is on translated second vector
       result
       nil))))


(defn dist-p2-v2 [[px py] [tx ty] [bx by]]
  "perpendicular distance of point and vector considering endpoints"
  (let [cross (isp-l2-l2 [tx ty][bx by][px py][(- by) bx])
        connv (sub-v2 [px py] cross)]
    (if (p2-in-v2? cross [tx ty] [bx by] 0)
      (length-v2 connv)
      ##Inf)))


(defn mirror-v2-bases [[ax ay] [vx vy]]
  "mirrors vector on axis, projects v on a, then adds the connecting vector of v and p to p"
  (let [[px py] (isp-l2-l2 [0 0] [ax ay] [vx vy] [ay (- ax)])
        [bx by] [(- px vx) (- py vy)]]
    [(+ px bx) (+ py by)]))


(defn dist-p2-p2-cubic [[ax ay][bx by]]
  "returns distance of two points based on x and y distances to avoid square root calculation"
  (+ (Math/abs (- bx ax)) (Math/abs (- by ay))))


(defn rotate-90-cw [ [x y] ]
  [y (- x)])


(defn rotate-90-ccw [ [x y] ]
  [(- y) x])

(defn rotate-up-v2 [ [x y] ]
  (if (< x 0) [(- y) x] [y (- x)]))
