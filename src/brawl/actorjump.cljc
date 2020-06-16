(ns brawl.actorjump
  (:require [brawl.actorwalk :as walk]
            [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))


(defn move-head-jump
  "move head point"
  [{:keys [facing squat-size kick-pressed] {{[hx hy] :p} :hip} :masses
    {{[ax ay] :p} :base_l {[bx by] :p} :base_r} :bases
    {legl :legl bodyl :bodyl headl :headl} :metrics
    {:keys [down up left right kick]} :control :as state}]
  (let [nx (* facing (/ (Math/abs (- bx ax )) 8.0 )) ; head move forward and backwards when stepping
        nnx (* facing squat-size 0.5) ; head move even forward when squatting
        ny (* squat-size 0.25) ; head should move lower when squatting
        jnx (if kick (* facing legl -0.3) 0.0) ; kick delta x
        neck [(+ hx nx nnx jnx) (- (+ hy ny) bodyl)]
        head [(+ hx nx nnx jnx) (- (+ hy ny) (+ bodyl headl))]]
    (-> state
        (assoc-in [:masses :neck :p] neck)
        (assoc-in [:masses :head :p] head))))


(defn move-hip-jump
  "move hip points, handle jumping"
  [{:keys [next jump-state] {{[hx hy] :p} :hip} :masses
    {{[ax ay] :p} :base_l {[bx by] :p} :base_r} :bases
    { legl :legl } :metrics :as state}]
  (let [x (+ ax (/ (- bx ax) 2))
        y (+ ay (/ (- by ay) 2))]
    (assoc-in state [:masses :hip :p] [x (- y (* legl 0.8))])))


(defn move-feet-jump
  "move active base towards target point"
  [{:keys [id color speed step-length facing action-sent commands]
    {{[hx hy] :p} :hip :as masses } :masses
    {base-order :order base-target :target} :base
    {base_l :base_l base_r :base_r} :bases
    {legl :legl runs :runs walks :walks} :metrics
    {kick :kick} :control
    :as state}
   surfaces]
  (let [foot_l (if kick [(+ hx (* legl facing) -10.0 ) (+ hy (* legl 0.5))] (:p base_l))
        foot_r (if kick [(+ hx (* legl facing)) (+ hy (* legl 0.5) -10.0)] (:p base_r))
        newcommands (if-not (and kick (not action-sent))
                      commands
                      (into commands [{:id id
                                       :text "attack"
                                       :base [hx hy]
                                       :target foot_l
                                       :radius 100.0
                                       :facing facing
                                       :color color
                                       :power 50.0}]))]
    (-> state
        (assoc :commands newcommands)
        (assoc :action-sent (if (and kick (not action-sent)) true action-sent))
        (assoc-in [:masses :foot_l :p] foot_l) 
        (assoc-in [:masses :foot_r :p] foot_r))))


(defn update-jump
  "jump state update, update base masses in the world, check if they reached ground" 
  [{:keys [bases masses speed] :as state}
   surfaces
   time
   delta]
  (let [newbases (-> bases 
                     (phys2/add-gravity [0.0 (* 0.5 delta)])
                     (phys2/move-masses surfaces (* 0.5 delta)))
        ground (and (:q (:base_l newbases) (:base_r newbases))) ;; check quisence of bases
        next-mode (cond
               ground :walk
               ;(< speed -15) :idle
               :else nil)
        result (cond-> state
                 next (assoc :next-mode next-mode)
                 true (assoc :bases newbases)
                 true (move-hip-jump)
                 true (move-feet-jump surfaces)
                 true (walk/move-knee-walk)
                 true (move-head-jump)
                 true (walk/move-hand-walk surfaces))]
    (if (= result nil) println "UPDATEJUMP ERROR!!!")
    result))
