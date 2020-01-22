(ns brawl.actor
  (:require [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))

(defn init [x y]
  {:mode "jump"
   :a_on_ground false
   :b_on_ground false   
   :speed [0.0 0.0]
   :state "walk"
   :facing 1.0

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

   :walk {:is_moving false
          :wants_to_jump false
          :vertical_direction 0
          :maxspeed 0
          :prevspeed 0
          :steplength 0
          :squatsize 0
          :breathangle 0
          :final_point [0 0]
          :activebase :base_a
          :passivebase :base_b
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


(defn updateskeleton [ {{{[txa tya] :p} :base_a
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


(defn newjumpstate [{:keys [mode bases masses speed walk facing] :as state}
                    {:keys [left right up down] :as control}
                    surfaces
                    time]
  (let [newbases (-> bases
                     (phys2/add-gravity [0.0 0.5])
                     (phys2/move-masses surfaces))
        a_on_ground (every? #(= % 0.0) (:d (:base_a newbases)))
        b_on_ground (every? #(= % 0.0) (:d (:base_b newbases)))
        newmode (cond
                  (and a_on_ground b_on_ground) "walk"
                  (< (speed 0) -15) "dead"
                  :else "jump")]
    (-> state
        (assoc :bases newbases)
        (assoc-in [:mode] newmode)
        (assoc-in [:a_on_ground] a_on_ground)
        (assoc-in [:b_on_ground] b_on_ground))))


(defn changefoot [{:keys [bases walk] :as state} surfaces sx]
  "swap activa and passive foot bases, set new target coordinate for active base"
  (let [{[bax bay] :p} (bases :base_a)
        {[bbx bby] :p} (bases :base_b)
        nabase (if (or (and (< bax bbx) (>= sx 0.0)) (and (> bax bbx) (< sx 0.0))) :base_a :base_b)
        npbase (if (or (and (< bax bbx) (>= sx 0.0)) (and (> bax bbx) (< sx 0.0))) :base_b :base_a)
        stepsize (+ (* (/ sx (Math/abs sx)) 40.0) (* sx 8.0))
        {[npbx npby] :p} (bases npbase)
        {[nabx naby] :p} (bases nabase)
        strans [(+ npbx stepsize) npby]
        sbupper [(- stepsize) (/ (Math/abs stepsize) 2.0)]
        sblower [(- stepsize) (-(/ (Math/abs stepsize) 2.0))]
        collided ;;(if (= (walk :vertical_direction) 1)
        (sort-by first < (concat
                          (phys2/get-colliding-surfaces strans sbupper 10.0 surfaces)
                          (phys2/get-colliding-surfaces strans sblower 10.0 surfaces)))
        surf (first collided)
        final_point (if surf
                      (nth surf 2)
                      strans)
        newpassivesurf (walk :activesurf)
        newactivesurf (if surf
                        (nth surf 1)
                        nil)
        ]
    ;;(println "st su sl coll surf" strans sbupper sblower collided surf)
    (assoc state :walk (-> walk
                           (assoc :activebase nabase)
                           (assoc :passivebase npbase)
                           (assoc :activesurf newactivesurf)
                           (assoc :passivesurf newpassivesurf)
                           (assoc :final_point final_point)
                           (assoc :is_moving true)))))


(defn movefoot [{:keys [bases walk] :as state} surfaces sx facing time]

  (let [stepv (math2/sub-v2 (walk :final_point) ((bases (walk :activebase)) :p))
        stepvl (math2/length-v2 stepv)
        nstepv (math2/resize-v2 stepv (* (Math/abs sx) time ))
        ntarget (math2/add-v2 ((bases (walk :activebase)) :p) nstepv)
        newbases (assoc-in bases [(walk :activebase) :p] ntarget)
        is_moving (if (< stepvl (* (Math/abs sx) time)) false true)
        ]
    (-> state
        (assoc :walk (assoc walk :is_moving is_moving))
        (assoc :bases newbases)
        (assoc-in [:speed 0] sx)
        (assoc :facing facing))))


(defn newwalkstate [{:keys [mode bases masses speed walk facing] :as state}
                    {:keys [left right up down] :as control  }
                    surfaces
                    time]
  (let [{:keys [is_moving wants_to_jump vertical_direction maxspeed prevspeed
                steplength squatsize breathangle final_point activebase passivebase]} walk
        maxspeed 20.0
        [sx sy] speed
        nsx (cond-> sx
              right (+ (* 0.4 time))
              left (- (* 0.4 time))
              (not (and left right)) (* 0.9))
        nnsx (cond
               (< nsx -10.0) -10.0
               (> nsx 10.0 ) 10.0
               :default nsx)
        newfacing (cond
                    (and (> nnsx 0.0 ) right) 1
                    (and (<= nnsx 0.0 ) left ) -1)]

    (if (and (not is_moving) (> (Math/abs nnsx) 0.1 ))
      ;; set new targets for bases
      (changefoot state surfaces nnsx)
      ;; move bases
      (if (and is_moving (> (Math/abs nnsx) 0.01))
        (movefoot state surfaces nnsx newfacing time)
        state))))


(defn newstate [{mode :mode :as state} control surfaces time]
  (let [newstate (cond-> state
                   (= mode "jump") (newjumpstate control surfaces time)
                   (= mode "walk") (newwalkstate control surfaces time)
                   :default identity)]
    (updateskeleton newstate)))
