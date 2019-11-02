(ns brawl.math2)


(defn segment2
  "creates segment 2d structure"
  [[x y][ z v]]
  {:trans [x y]
   :basis [(- z x) (- v y)]})


(defn cross_line2
  "line intersection by linear equation
  Ax + By = C
  A = y2 - y1
  B = x1 - x2
  C = A * x1 + B * y1"
  [[x_ta y_ta :as trans_a]
   [x_ba y_ba :as basis_a]
   [x_tb y_tb :as trans_b]
   [x_bb y_bb :as basis_b]]

  ;; if no length there is no direction
  (if (or
       (and (= x_ba 0)(= y_ba 0))
       (and (= x_bb 0)(= y_bb 0)))
    nil
    (let [aA y_ba
          aB (* x_ba -1)
          bA y_bb
          bB (* x_bb -1)
          dt (-(* bA aB)(* bB aA))]
      (

       ;; if determinant is 0 they are parallel
       if (= dt 0)
       nil
       ( let [aC (+(* x_ta aA)(* y_ta aB))
              bC (+(* x_tb bA)(* y_tb bB))
              x (/(-(* aB bC)(* bB aC)) dt)
              y (/(-(* bA aC)(* aA bC)) dt)]
        [x y] )))))


(defn inside_vec2
  "check if p is inside vector defined by trans and basis"
  [[tx ty] [bx by] [px py]]
  (let [dx (- px tx)
        dy (- py ty)
        xok (if (= bx 0)
              (< (Math/abs dx) 0.00001)
              (let [rx (/ dx bx)]
                (and (> rx 0.0)(< rx 1.0))))
        yok (if (= by 0)
              (< (Math/abs dy) 0.00001)
              (let [ry (/ dy by)]
                (and (> ry 0.0)(< ry 1.0))))]
    (and xok yok)))


(defn cross_vec2
  "checks vector crossing"
  [transa basisa transb basisb]
  (let [cross (cross_line2 transa basisa transb basisb)]
    (if (= cross nil)
     nil
     (if (and
          (inside_vec2 transa basisa cross)
          (inside_vec2 transb basisb cross))
       cross
       nil))))


(defn mirror_vec2
  "mirrors vector on axis"
  [[ax ay :as axis ] [bx by :as vector]]
  (let [[px py :as projpt]
        ( cross_line2 [0 0] [ax ay] [bx by] [ay (* -1 ax )])
        [vx vy :as projvec]
        [ (- px bx) (- py by)]]
    [(+ px vx) (+ py vy)]))



(defn length_vec2 [ [ax ay] ]
  (Math/sqrt (+ (* ax ax) (* ay ay))))


(defn resize_vec2 [[x y] size]
  (let [ratio (/ size (length_vec2 [x y]))]
    [(* x ratio) (* y ratio)]))


(defn scale_vec2 [ [x y] ratio ]
  [(* x ratio) (* y ratio)])


(defn sub_vec2 [ [ax ay] [bx by] ]
  [(- ax bx) (- ay by)])


(defn add_vec2 [ [ax ay] [bx by] ]
  [(+ ax bx) (+ ay by)])


(defn rotate_90_cw [ [x y] ]
  [y (- x)])

(defn rotate_90_ccw [ [x y] ]
  [(- y) x])


;; optimize by removing sqrt
(defn triangle_with_bases [ va vb side dir ]
  (let [dirv (sub_vec2 vb va)
        half (scale_vec2 dirv 0.5)
        size (length_vec2 half) 
        result (if (< size side)
                 (let [needed (Math/sqrt (- (* side side) (* size size)))
                       normal (resize_vec2 [(* dir (- (half 1))) (* dir (half 0))] needed)]
                   (add_vec2 (add_vec2 va half) normal))
                 (add_vec2 va half))]
    result
    ))
