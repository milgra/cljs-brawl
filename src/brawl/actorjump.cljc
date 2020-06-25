(ns brawl.actorjump
  (:require [brawl.actorwalk :as walk]
            [mpd.math2 :as math2]
            [mpd.phys2 :as phys2]))


(defn move-head-jump
  "move head point"
  [actor]
  (let [{:keys [facing kick-pressed]} actor
        {:keys [squat-size]} (:walk actor)
        {:keys [legl bodyl headl]} (:metrics actor)
        {:keys [kick]} (:control actor)
        {{[hx hy] :p} :hip} (:masses actor)
        {{[ax ay] :p} :base_l {[bx by] :p} :base_r} (:bases actor)
        nx (* facing (/ (Math/abs (- bx ax )) 8.0 )) ; head move forward and backwards when stepping
        nnx (* facing squat-size 0.5) ; head move even forward when squatting
        ny (* squat-size 0.25) ; head should move lower when squatting
        jnx (if kick (* facing legl -0.3) 0.0) ; kick delta x
        neck [(+ hx nx nnx jnx) (- (+ hy ny) bodyl)]
        head [(+ hx nx nnx jnx) (- (+ hy ny) (+ bodyl headl))]]
    (-> actor
        (assoc-in [:masses :neck :p] neck)
        (assoc-in [:masses :head :p] head))))


(defn move-hip-jump
  "move hip points, handle jumping"
  [actor]
  (let [{:keys [next]} actor
        {{[hx hy] :p} :hip} (:masses actor)
        {{[ax ay] :p} :base_l {[bx by] :p} :base_r} (:bases actor)
        {:keys [legl]} (:metrics actor)
        x (+ ax (/ (- bx ax) 2))
        y (+ ay (/ (- by ay) 2))]
    (assoc-in actor [:masses :hip :p] [x (- y (* legl 0.8))])))


(defn move-feet-jump
  "move active base towards target point"
  [actor surfaces]
  (let [{:keys [id color facing commands]} actor
        {:keys [action-sent]} (:attack actor)
        {:keys [base_l base_r]} (:bases actor)
        {:keys [legl]} (:metrics actor)
        {:keys [kick]} (:control actor)
        {{[hx hy] :p} :hip} (:masses actor)
        foot_l (if kick [(+ hx (* legl facing) -10.0 ) (+ hy (* legl 0.5))] (:p base_l))
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
    (-> actor
        (assoc :commands newcommands)
        (assoc-in [:attack :action-sent] (if (and kick (not action-sent)) true action-sent))
        (assoc-in [:masses :foot_l :p] foot_l) 
        (assoc-in [:masses :foot_r :p] foot_r))))


(defn update-jump
  "jump state update, update base masses in the world, check if they reached ground" 
  [actor surfaces time delta]
  (let [{:keys [health bases masses speed next-mode]} actor
        {:keys [jump-lethal jump-speed]} (:walk actor)
        newbases (-> bases 
                     (phys2/add-gravity [0.0 (* 0.5 delta)])
                     (phys2/move-masses surfaces (* 0.5 delta)))
        ground (and (:q (:base_l newbases) (:base_r newbases))) ;; check quisence of bases
        [sx sy] (:d (:base_l bases))
        lethal-new (if (> sy 20) true jump-lethal)
        speed-new (if (> sy 20) sy jump-speed)
        mode-new (if ground :walk next-mode)]
    (if (and ground jump-lethal)
      (let [masses-new (reduce (fn [res [id mass]] (update-in res [id :d 1] + speed-new)) masses masses)  ; reset mass directions for next rag
            actor-new (if (= (:id actor) :hero) (update actor :commands conj {:text "show-wasted"}) actor)]
        (-> actor-new
            (assoc :health -1)
            (assoc :masses masses-new)
            (assoc :next-mode :rag)))
      (-> actor
          (assoc-in [:walk :jump-lethal] lethal-new)
          (assoc-in [:walk :jump-speed] speed-new)
          (assoc :next-mode mode-new)
          (assoc :bases newbases)
          (move-hip-jump)
          (move-feet-jump surfaces)
          (walk/move-knee-walk)
          (move-head-jump)
          (walk/move-hand-walk surfaces)))))
