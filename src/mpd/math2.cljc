(ns mpd.math2)


(defn add-v2 [[ax ay][bx by]]
  [(+ ax bx) (+ ay by)])


(defn sub-v2 [[ax ay][bx by]]
  [(- ax bx) (- ay by)])


(defn dot-v2 [[ax ay][bx by]]
  (+ (* ax bx) (* ay by)))

 
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


(defn rad-to-degree [rad]
  (/ (* 180 rad) 3.14))


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


(defn norm-p2-l2
  "creates vector normal to line defined by [t b] from p"
  [[px py] [tx ty] [bx by]]
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


(defn point-outside-rect
  "check if point outside rect defined by two points"
  [[ax ay] [bx by] [px py] r]
  (or (and (< px (- ax r)) (< px (- bx r)))
      (and (> px (+ ax r)) (> px (+ bx r)))
      (and (< py (- ay r)) (< py (- by r)))
      (and (> py (+ ay r)) (> py (+ by r)))))


(defn rect-outside-rect
  "check if rect outside rect defined by two points"
  [[ax ay][bx by][cx cy][dx dy]]
  (or (and (< ax cx) (< ax dx) (< bx cx) (< bx dx))
      (and (> ax cx) (> ax dx) (> bx cx) (> bx dx)) 
      (and (< ay cy) (< ay dy) (< by cy) (< by dy))
      (and (> ay cy) (> ay dy) (> by cy) (> by dy))))


(defn get-line-side
  "checks that pc is on which side of the line defined by pa pb using determinant of vectors (AB,AC)
  negative is left side, zero is on line, positive is right side"
  [[ax ay][bx by][cx cy]]
  (- (* (- bx ax ) (- cy ay)) (* (- by ay) (- cx ax))))


(defn collide-v2-v2
  "check intersection points of the two lines, if it is in the bounding box of the two vectors return isp
  if not then check if point is on the right side of the vector and the projection point is in the bounding box
  of the second vector"
  [pa [ax ay :as da] pb [bx by :as db] radius]
  (let [end-a (add-v2 pa da)
        end-b (add-v2 pb db)]
    (if (rect-outside-rect pa end-a pb end-b)
      ;; rects don't cover each other
      nil
      ;; rects cover each other
      (if (or (and (= ax 0)(= ay 0)) (and (= bx 0)(= by 0)))
        ;; invalid lines or no collision in rects
        nil
        ;; valid lines
        (let [isp (isp-l2-l2 pa da pb db)
              normal [by (- bx)]
              normal-min (resize-v2 normal 0.2)]
          (if (or
               (point-outside-rect pb end-b isp 0.1)
               (point-outside-rect pa end-a isp 0.1))
            ;; isp is outside vectors, check if projection point is on the right side of the vector
            (let [prj (isp-l2-l2 pb db pa [by (- bx)])]
              (if (point-outside-rect pb end-b prj 0.1)
                ;; projection point is outside surface
                nil
                ;; projection point is inside surface, check if direction is okay
                (let [tos (sub-v2 prj pa) ;; to surface
                      dot (dot-v2 tos normal)] ;; dot product of to surface vector and surface normal vector
                  (if (and (> dot 0) (< (length-v2 tos) radius))
                    ;; to surface vector is in the same direction as normal vector and in radius distance
                    (add-v2 prj normal-min) ;; push over surface a little bit to avoid intersection in next iteration
                    ;; to surface vector is in the opposing direction
                    nil))))
            ;; isp is inside vectors, return with it
            (add-v2 isp normal-min))))))) ;; push over surface


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
