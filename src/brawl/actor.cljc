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
          :passivebase :base_b }
   })


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
  (let [ankle_a [txa tya]
        ankle_b [txb tyb]
        hipx (+ txa (/ (- txb txa) 2))
        hipy (- (+ tya (/ (- tyb tya) 2)) 50.0)
        neck [hipx (- hipy 50.0)]
        head [hipx (- hipy 70.0)]
        knee_a (triangle_with_bases ankle_a [hipx hipy] 30.0 1.0)
        knee_b (triangle_with_bases ankle_b [hipx hipy] 30.0 1.0)
        hand_a [(+ hipx 40.0) (- hipy 50.0)]
        hand_b [(+ hipx 30.0) (- hipy 40.0)]
        elbow_a (triangle_with_bases neck hand_a 30.0 1.0)
        elbow_b (triangle_with_bases neck hand_b 30.0 1.0)]

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


(defn newwalkstate [{:keys [mode bases masses speed walk facing] :as state}
                    {:keys [left right up down] :as control  }
                    surfaces
                    time]
  (let [{:keys [is_moving wants_to_jump vertical_direction maxspeed prevspeed
                steplength squatsize breathangle final_point activebase passivebase]} walk
        maxspeed 10.0
        [sx sy] speed
        nsx (cond-> sx
              right (+ (* 0.3 time))
              left (- (* 0.3 time))
              (not (and left right)) (* 0.9))
        nnsx (cond
               (< nsx -10.0) -10.0
               (> nsx 10.0 ) 10.0
               :default nsx)
        newfacing (cond
                    (and (> nnsx 0.0 ) right) 1
                    (and (< nnsx 0.0 ) left ) 0)]

    (if (and (not is_moving) (> (Math/abs nnsx) 0.1 ))
      ;; set new targets for bases
      (let [{[bax bay] :p} (bases :base_a)
            {[bbx bby] :p} (bases :base_b)
            nabase (if (or (and (< bax bbx) (>= nnsx 0.0)) (and (> bax bbx) (< nnsx 0.0))) :base_a :base_b)
            npbase (if (or (and (< bax bbx) (>= nnsx 0.0)) (and (> bax bbx) (< nnsx 0.0))) :base_b :base_a)
            stepsize (+ (* (/ nnsx (Math/abs nnsx)) 40.0) (* nnsx 8.0))
            {[npbx npby] :p} (bases npbase)
            {[nabx naby] :p} (bases nabase)
            strans [(+ npbx stepsize) npby]
            sbupper [(- stepsize) (/ (Math/abs stepsize) 2.0)]
            sblower [(- stepsize) (-(/ (Math/abs stepsize) 2.0))]
            collided (if (= vertical_direction 1)
                       (map second (sort-by first < (phys2/get-colliding-surfaces strans sbupper 10.0 surfaces)))
                       (map second (sort-by first < (phys2/get-colliding-surfaces strans sblower 10.0 surfaces))))
            surf (first collided)
            final_point (if surf
                          (nth surf 2)
                          strans)]
        (println "st su sl coll" strans sbupper sblower collided)
        (assoc state :walk (-> walk
                               (assoc :activebase nabase)
                               (assoc :passivebase npbase)
                               (assoc :final_point final_point)
                               (assoc :is_moving true))))

      ;; move bases
      (if (and is_moving (> (Math/abs nnsx) 0.01))
        (let [stepv (math2/sub-v2 final_point ((bases activebase) :p))
              stepvl (math2/length-v2 stepv)
              nstepv (math2/resize-v2 stepv (* (Math/abs nnsx) time ))
              ntarget (math2/add-v2 ((bases activebase) :p) nstepv)
              newbases (assoc-in bases [activebase :p] ntarget)
              is_moving (if (< stepvl (* (Math/abs nnsx) time)) false true)
              ]
          (-> state
              (assoc :walk (assoc walk :is_moving is_moving))
              (assoc :bases newbases)
              (assoc :speed [nnsx sy])
              (assoc :facing newfacing)))
        state))))


(defn newstate [{mode :mode :as state} control surfaces time]
  (let [newstate (cond-> state
                   (= mode "jump") (newjumpstate control surfaces time)
                   (= mode "walk") (newwalkstate control surfaces time)
                   :default identity)]
    (updateskeleton newstate)))
    

(defn getpoints [{masses :masses bases :bases}]
  (concat (map :p (vals masses)) (map :p (vals bases))))


(defn getlines [{{:keys [head neck hip elbow_a elbow_b hand_a hand_b knee_a knee_b ankle_a ankle_b]} :masses}]
  [(:p head) (:p neck)
   (:p neck) (:p hip)
   (:p hip) (:p knee_a)
   (:p hip) (:p knee_b)
   (:p knee_a ) (:p ankle_a)
   (:p knee_b ) (:p ankle_b)
   (:p neck ) (:p elbow_a)
   (:p neck ) (:p elbow_b)
   (:p elbow_a ) (:p hand_a)
   (:p elbow_b ) (:p hand_b)])


(defn gen-tube-triangles [ points sizes]

  (loop [rempts points
         remszs sizes
         result []]
    (if (= 2 (count rempts))
      ;; close tube
      (let [pa (nth rempts 0)
            pb (nth rempts 1)
            sa (nth remszs 0)
            sb (nth remszs 1)
            ab (math2/sub-v2 pb pa)
            nlsa (math2/add-v2 pa (math2/resize-v2( math2/rotate-90-ccw ab) sa))
            nrsa (math2/add-v2 pa (math2/resize-v2( math2/rotate-90-cw ab) sa))
            nlea (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-ccw ab) sb))
            nrea (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-cw ab) sb))]
        (conj result
              nlsa nrsa nrea
              nlsa nrea nlea))
      ;; add rect and joint triangle
      (let [pa (nth rempts 0)
            pb (nth rempts 1)
            pc (nth rempts 2)
            sa (nth remszs 0)
            sb (nth remszs 1)
            ab (math2/sub-v2 pb pa)
            bc (math2/sub-v2 pc pb)
            nlsa (math2/add-v2 pa (math2/resize-v2( math2/rotate-90-ccw ab) sa))
            nrsa (math2/add-v2 pa (math2/resize-v2( math2/rotate-90-cw ab) sa))
            nlea (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-ccw ab) sb))
            nrea (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-cw ab) sb))
            nlsb (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-ccw bc) sb))
            nrsb (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-cw bc) sb))]
        (recur (rest rempts)
               (rest remszs)
               (conj result
                     nlsa nrsa nrea
                     nlsa nrea nlea
                     nlea nlsb pb
                     nrea nrsb pb))))))

;; TODO when on ground, lay on ground surface
(defn gen-foot-triangles [pa pb size facing]
  (let [ab (math2/sub-v2 pb pa)
        abbig (math2/add-v2 pa (math2/scale-v2 ab 1.2))
        leftp (if (= facing 1)
                (math2/add-v2 abbig (math2/resize-v2 (math2/rotate-90-cw ab) 10.0))
                (math2/add-v2 abbig (math2/resize-v2 (math2/rotate-90-cw ab) 20.0)))
        rightp (if (= facing 1)
                (math2/add-v2 abbig (math2/resize-v2 (math2/rotate-90-ccw ab) 20.0))
                (math2/add-v2 abbig (math2/resize-v2 (math2/rotate-90-ccw ab) 10.0)))
        topp (if (= facing 1)
                (math2/add-v2 leftp (math2/resize-v2 ab -18.0))
                (math2/add-v2 rightp (math2/resize-v2 ab -18.0)))]
    [topp leftp rightp]))


(defn gen-head-triangles [pa pb facing]
  (let [ab (math2/sub-v2 pb pa)
        nlsa (math2/add-v2 pa (math2/resize-v2( math2/rotate-90-ccw ab) 10.0))
        nrsa (math2/add-v2 pa (math2/resize-v2( math2/rotate-90-cw ab) 10.0))
        nlea (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-ccw ab) 11.0))
        nrea (math2/add-v2 pb (math2/resize-v2( math2/rotate-90-cw ab) 11.0))
        ab23 (math2/add-v2 pa (math2/scale-v2 ab 0.66))
        nose (if (= 1 facing)
               (math2/add-v2 ab23 (math2/resize-v2 (math2/rotate-90-ccw ab) 15.0))
               (math2/add-v2 ab23 (math2/resize-v2 (math2/rotate-90-cw ab) 15.0)))]
    (if (= 1 facing)
      [nlsa nrsa nrea nlsa nrea nose nose nlea nrea]
      [nlsa nrsa nose nlsa nose nlea nose nrea nlea])))


(defn get-skin-triangles
  [{{:keys [head neck hip elbow_a elbow_b hand_a hand_b knee_a knee_b ankle_a ankle_b facing]} :masses
    {:keys [headw neckw bodyw hipw legw]} :metrics }]

  ;; foot

  (concat []
          ;; feet
          (gen-foot-triangles (knee_a :p) (ankle_a :p) 5.0 facing)
          (gen-foot-triangles (knee_b :p) (ankle_b :p) 5.0 facing)
          ;; legs
          (gen-tube-triangles [(:p neck) (:p hip) (:p knee_a) (:p ankle_a)] [1.0 hipw legw legw])
          (gen-tube-triangles [(:p neck) (:p hip) (:p knee_b) (:p ankle_b)] [6.0 hipw legw legw])
          ;; arms
          (gen-tube-triangles [(:p neck) (:p elbow_a) (:p hand_a)] [5.0 5.0 5.0])
          (gen-tube-triangles [(:p neck) (:p elbow_b) (:p hand_b)] [5.0 5.0 5.0])
          ;; body
          (gen-tube-triangles [(:p head) (:p neck) (:p hip)] [neckw neckw hipw])
          ;; head
          (gen-head-triangles (:p head) (:p neck) facing)))
