(ns brawl.actor
  (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))

(declare update-jump)
(declare update-walk)
(declare update-idle)

(defn init [x y]
  {:speed [0.0 0.0]
   :facing 1.0
   :update-fn update-jump
   
   :bases  {:base_a (phys2/mass2 (+ x 20.0) y 4.0 10.0 0.0)
            :base_b (phys2/mass2 (- x 20.0) y 4.0 10.0 0.0)}

   :masses {:head (phys2/mass2 x y 4.0 1.0 1.0)
            :neck (phys2/mass2 x y 4.0 1.0 1.0)
            :hip  (phys2/mass2 x y 4.0 1.0 1.0)

            :hand_a (phys2/mass2 x y 4.0 1.0 1.0)
            :hand_b (phys2/mass2 x y 4.0 1.0 1.0)
            
            :elbow_a (phys2/mass2 x y 4.0 1.0 1.0)
            :elbow_b (phys2/mass2 x y 4.0 1.0 1.0)
            
            :knee_a (phys2/mass2 x y 4.0 1.0 1.0)
            :knee_b (phys2/mass2 x y 4.0 1.0 1.0)
            
            :ankle_a (phys2/mass2 x y 4.0 1.0 1.0)
            :ankle_b (phys2/mass2 x y 4.0 1.0 1.0)}

   :metrics {;; lengths
             :headl 20.0
             :bodyl 50.0
             :arml 50.0
             :legl 60.0
             ;; widths
             :headw 40.0
             :neckw 4.0
             :armw 4.0
             :bodyw 6.0
             :hipw 6.0
             :legw 6.0
             ;; speed
             :walks 0.6
             :runs 0.4
             :punchs 7.0
             :kicks 0.2
             ;; powers
             :maxp 100.0
             :hitp 30.0
             :kickp 30.0
             ;; health level xp todo on level step ability to pick up bodies
             :maxh 100.0
             :maxl 10
             ;; color
             :col 0xFF0000FF
             :cola 0xAA0000FF
             :colb 0x00AA00FF
             :colc 0x0000AAFF
             :cold 0xAA00AAFF}

   :jump {
          :foot-on-ground {:a false :b false}
          }
   
   :walk {:dostep! true ; force step at start
          :final_point [0 0]
          :foot-order {:active :base_a :passive :base_b}
          :foot-surfaces {:active nil :passive nil}
          
          :is_moving false  ; remove later
          :wants_to_jump false
          :vertical_direction 0
          :maxspeed 0
          :prevspeed 0
          :steplength 0
          :squatsize 0
          :breathangle 0

          :activesurf nil
          :passivesurf nil}})


(defn triangle_with_bases [va vb side dir]
  (let [dirv (math2/sub-v2 vb va)
        half (math2/scale-v2 dirv 0.5)
        size (math2/length-v2 half) 
        result (if (< size side)
                 (let [needed (Math/sqrt (- (* side side) (* size size)))
                       normal (math2/resize-v2 [(* dir (- (half 1))) (* dir (half 0))] needed)]
                   (math2/add-v2 (math2/add-v2 va half) normal))
                 (math2/add-v2 va half))]
    result))


(defn update-skeleton [ {{{[txa tya] :p} :base_a
                         {[txb tyb] :p} :base_b} :bases :as  state }]
  (let [facing (state :facing)
        ankle_a [txa tya]
        ankle_b [txb tyb]
        hipx (+ txa (/ (- txb txa) 2))
        hipy (- (+ tya (/ (- tyb tya) 2)) 50.0)
        neck [hipx (- hipy 50.0)]
        head [hipx (- hipy 70.0)]
        knee_a (triangle_with_bases ankle_a [hipx hipy] 30.0 facing)
        knee_b (triangle_with_bases ankle_b [hipx hipy] 30.0 facing)
        hand_a [(+ hipx (* facing 40.0)) (- hipy 50.0)]
        hand_b [(+ hipx (* facing 30.0)) (- hipy 40.0)]
        elbow_a (triangle_with_bases neck hand_a 30.0 facing)
        elbow_b (triangle_with_bases neck hand_b 30.0 facing)]
  (-> state
      (assoc-in [:masses :ankle_a :p] ankle_a) 
      (assoc-in [:masses :ankle_b :p] ankle_b) 
      (assoc-in [:masses :knee_a :p] knee_a ) 
      (assoc-in [:masses :knee_b :p] knee_b ) 
      (assoc-in [:masses :hip :p] [hipx hipy])
      (assoc-in [:masses :neck :p] neck)
      (assoc-in [:masses :head :p] head)
      (assoc-in [:masses :hand_a :p] hand_a) 
      (assoc-in [:masses :hand_b :p] hand_b) 
      (assoc-in [:masses :elbow_a :p] elbow_a) 
      (assoc-in [:masses :elbow_b :p] elbow_b))))


(defn update-idle [state] state)


(defn update-jump [{:keys [bases jump speed] :as state}
                  {:keys [left right up down] :as control}
                  surfaces
                  time]
  (let [newbases (-> bases
                     (phys2/add-gravity [0.0 0.5])
                     (phys2/move-masses surfaces))
        a_on_ground (every? #(= % 0.0) (:d (:base_a newbases)))
        b_on_ground (every? #(= % 0.0) (:d (:base_b newbases)))
        update-fn (cond
                  (and a_on_ground b_on_ground) update-walk
                  (< (speed 0) -15) update-idle
                  :else update-jump)]
    ; (println "newbases" (map #(:p (val %)) newbases))
    (-> state
        (assoc :bases newbases)
        (assoc :update-fn update-fn)
        (assoc :jump {assoc jump :foot-on-ground {:a a_on_ground :b b_on_ground}}))))


(defn movefoot [{:keys [bases walk]
                 [sx sy] :speed :as state}
                surfaces
                time]
  (if (> (Math/abs sx) 0.1)
    (let [activekw (get-in walk [:foot-order :active])
          activept ((bases activekw) :p)

          stepv (math2/sub-v2 (walk :final_point) activept)
          stepvl (math2/length-v2 stepv)

          nstepv (math2/resize-v2 stepv (* (Math/abs sx) time ))
          ntarget (math2/add-v2 activept nstepv)

          newbases (assoc-in bases [activekw :p] ntarget)
          is_moving (if (< stepvl (* (Math/abs sx) time)) false true)
          dostep! (not is_moving)]
      ; (println "is moving" is_moving "dostep!" dostep!)
      (-> state
          (assoc :walk (-> walk
                           (assoc :is_moving is_moving)
                           (assoc :dostep! dostep!)))
          (assoc :bases newbases)))
    (-> state
        (assoc :walk (-> walk
                         (assoc :is_moving false)
                         (assoc :dostep! true))))))


(defn get-step-triangle [[x y] speed]
  (let [stepsize (cond
                   (and (> speed -1.0) (<  speed 0.0)) -10.0
                   (and (< speed  1.0) (>= speed 0.0))  10.0
                   :else (+ (* (/ speed (Math/abs speed ) 40.0 ) ) (* speed 8.0)))
        A [(+ x stepsize) y]
        B [(- stepsize) (/ (Math/abs stepsize) 2.0)]
        C [(- stepsize) (-(/ (Math/abs stepsize) 2.0))]]
    {:A A :B B :C C }))


(defn get-foot-order [bases speed ]
  (let [{[bax bay] :p} (bases :base_a)
        {[bbx bby] :p} (bases :base_b)]
    (if (or (and (< bax bbx) (>= speed 0.0)) (and (> bax bbx) (< speed 0.0)))
      {:active :base_a :passive :base_b}
      {:active :base_b :passive :base_a})))


(defn stepfoot [{bases :bases
                 [sx sy] :speed
                 {dostep! :dostep! :as walk} :walk :as state} surfaces]
  (if (and dostep! (> (Math/abs sx) 0.1))
    (let [foot-order (get-foot-order bases sx)
          step-triangle (get-step-triangle (:p (bases (:passive foot-order))) sx)
          collided (sort-by first < (concat
                                     (phys2/get-colliding-surfaces (:A step-triangle) (:B step-triangle) 10.0 surfaces)
                                     (phys2/get-colliding-surfaces (:A step-triangle) (:C step-triangle) 10.0 surfaces)))
          surf (first collided)
          final_point (if surf
                        (nth surf 1)
                        (:A step-triangle))
          newpassivesurf (walk :activesurf)
          newactivesurf (if surf
                          (nth surf 1)
                          nil)]
      ; (println "stepfoot foor-order" foot-order "step-triangle" step-triangle "collided" collided)

      (assoc state :walk
             (-> walk
                 (assoc :dostep! false)
                 (assoc :is_moving true)
                 (assoc :foot-order foot-order)
                 (assoc :foot-surfaces {:active newactivesurf :passive newpassivesurf})
                 (assoc :final_point final_point))))
    state))


(defn update-speed [{[sx sy] :speed facing :facing :as state}
                   {:keys [left right up down]}
                   time]
  (let [nsx (cond-> sx
              right (max (+ (* 0.4 time)) 10.0)
              left  (min (- (* 0.4 time)) -10.0)
              :else (* 0.9))
        dir (cond
              (and (> nsx 0.0 ) right) 1
              (and (< nsx 0.0 ) left ) -1
              :else facing)]
    (-> state
        (assoc :speed [nsx sy])
        (assoc :facing dir)))) ; TODO replace facing with dir


(defn update-walk [state control surfaces time]
  (-> state
      (update-speed control time)
      (stepfoot surfaces)
      (movefoot surfaces time)))


(defn update-actor [{mode :mode update-fn :update-fn :as state} control surfaces time]
  (-> state
      (update-fn control surfaces time)
      (update-skeleton)))
