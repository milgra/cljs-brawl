(ns ^:figwheel-hooks brawl.core
  (:require
    [brawl.actor :as actor]
    [brawl.actorskin :as actorskin]
    [brawl.audio :as audio]
    [brawl.defaults :as defaults]
    [brawl.floatbuffer :as floatbuffer]
    [brawl.gun :as gun]
    [brawl.layouts :as layouts]
    [brawl.metrics :as metrics]
    [brawl.particle :as particle]
    [brawl.ui :as brawlui]
    [brawl.webgl :as webgl]
    [brawl.world :as world]
    [cljs-webgl.context :as context]
    [cljs.core.async :refer [<! chan put! take! poll!]]
    [goog.dom :as dom]
    [goog.events :as events]
    [gui.math4 :as math4]
    [gui.webgl :as uiwebgl])
  (:import
    (goog.events
      EventType)))


(defn resize-context!
  "resize canvas on window resize"
  []
  (let [canvas (dom/getElement "main")
        context (context/get-context canvas)
        rect (.getBoundingClientRect canvas)
        width (.-width rect)
        height (.-height rect)
        ratio (or (.-devicePixelRatio js/window) 1.0)]
    (dom/setProperties canvas (clj->js {:width (* width ratio)
                                        :height (* height ratio)}))))


(defn load-font!
  "load external font"
  [state name url]
  (let [font (js/FontFace. name (str "url(" url ")"))]
    (.then
      (.load font)
      (fn []
        (.add (.-fonts js/document) font)
        (put! (:msgch state) {:id "redraw-ui"})))))


(defn init-events!
  "start event listening"
  [state]
  (let [key-codes (atom {})
        mouse-down (atom false)]

    (events/listen
      js/window
      EventType.RESIZE
      (fn [event]
        (put! (:msgch state) {:id "resize"})
        (resize-context!)))

    (events/listen
      js/document
      EventType.KEYDOWN
      (fn [event]
        (let [code (.-keyCode event)
              prev (get @key-codes code)]
          (swap! key-codes assoc code true)
          (if (not prev) (put! (:msgch state) {:id "key" :code (.-keyCode event) :value true})))))

    (events/listen
      js/document
      EventType.KEYUP
      (fn [event]
        (let [code (.-keyCode event)
              prev (get @key-codes code)]
          (swap! key-codes assoc code false)
          (if prev (put! (:msgch state) {:id "key" :code (.-keyCode event) :value false})))))

    (events/listen
      js/document
      EventType.POINTERDOWN
      (fn [event]
        (swap! mouse-down not)
        (put! (:msgch state) {:id "mouse" :type "down" :point [(.-clientX event) (.-clientY event)]})))

    (events/listen
      js/document
      EventType.POINTERUP
      (fn [event]
        (swap! mouse-down not)
        (put! (:msgch state) {:id "mouse" :type "up" :point [(.-clientX event) (.-clientY event)]})))

    (events/listen
      js/document
      EventType.POINTERMOVE
      (fn [event]
        (if @mouse-down (put! (:msgch state) {:id "mouse" :type "down" :point [(.-clientX event) (.-clientY event)]}))))))


(defn draw-ui
  "draw ui elements with ui-drawer"
  [state]
  (let [{:keys [projection]} (:ui state)
        {:keys [ui-drawer]} state]
    (assoc state :ui-drawer (uiwebgl/draw! ui-drawer projection))))


(defn collect-triangles
  [buffer actors guns variation view-rect]
  (-> buffer
    (floatbuffer/empty!)
    ((partial reduce (fn [oldbuf [id actor]]
                       (if (not= id :hero) (actorskin/get-skin-triangles oldbuf actor variation view-rect) oldbuf))) actors)
    ((partial reduce (fn [oldbuf [id gun]] (gun/get-skin-triangles gun oldbuf view-rect))) guns)
    (actorskin/get-skin-triangles (:hero actors) variation view-rect)))


(defn collect-points
  [buffer actors particles view-rect physics]
  (cond-> buffer
    true (floatbuffer/empty!)
    true ((partial reduce (fn [oldbuf particle] (particle/get-point particle oldbuf))) particles)
    physics ((partial reduce (fn [oldbuf [id actor]] (actorskin/get-points actor oldbuf view-rect))) actors)))


(defn collect-lines
  [buffer actors surfacelines view-rect physics]
  (cond-> buffer
    true (floatbuffer/empty!)
    physics ((partial reduce (fn [oldbuf [id actor]] (actorskin/get-lines actor oldbuf view-rect))) actors)
    physics (floatbuffer/append! surfacelines)))


(defn draw-world
  "draws background, actors, masses with projection"
  [state frame]
  (let [{:keys [world-drawer buffer physics]} state
        {:keys [actors guns particles surfacelines view-rect projection] :as world} (:world state)]
    (if-not (:inited world)
      state
      (let [variation (Math/floor (mod (/ frame 10.0) 3.0))
            triangles (collect-triangles buffer actors guns variation view-rect)]
        (webgl/clear! world-drawer)
        (webgl/draw-shapes! world-drawer projection variation)
        (webgl/draw-triangles! world-drawer projection triangles)
        (let [points (collect-points triangles actors particles view-rect physics)]
          (webgl/draw-points! world-drawer projection points)
          (let [buffer-line (collect-lines points actors surfacelines view-rect physics)]
            (if physics (webgl/draw-lines! world-drawer projection buffer-line))
            (-> state
              (assoc :world-drawer world-drawer)
              (assoc-in [:world :view-rect] view-rect)
              (assoc :buffer buffer-line))))))))


(defn update-controls
  "set up control state based on keycodes"
  [state msg]
  (if-not (and msg (= (:id msg) "key"))
    state
    (let [{:keys [keycodes controls theme-started sounds]} state
          new-codes (assoc keycodes (:code msg) (:value msg))
          new-controls {:left (new-codes 37)
                        :right (new-codes 39)
                        :up (new-codes 38)
                        :down (new-codes 40)
                        :punch (new-codes 70)
                        :shoot (new-codes 86)
                        :run (new-codes 32)
                        :kick (new-codes 83)
                        :block (new-codes 68)}]

      (when (not theme-started)
        (set! (.-loop (:theme sounds)) true)
        (.play (:theme sounds)))

      (-> state
        (assoc :theme-started true)
        (assoc :keycodes new-codes)
        (assoc :controls new-controls)))))


(defn simulate
  "simulation step"
  [state time]
  (loop [old-time (min (:gametime state) 1660) ;; shouldn't go over 10 steps
         old-state state]
    (if (< old-time 16.6)
      (assoc old-state :gametime old-time)
      (let [message (poll! (:msgch state))
            new-state (-> old-state
                          ;; get controls
                        (update-controls message)
                          ;; world
                        (world/execute-commands time)
                        (world/reset-world message time)
                        (world/update-world message time 1.0)
                          ;; ui
                        (brawlui/execute-commands message)
                        (brawlui/update-ui message time 1.0))]
        (recur (- old-time 16.6) new-state)))))


(defn animate
  "main runloop, syncs animation to display refresh rate"
  [state draw-fn]
  (letfn [(loop
            [prestate frame]
            (fn [time]
              (let [delta (- time (:time prestate))
                    state (if (< delta time)
                            (draw-fn prestate frame time delta)
                            prestate)]
                (.requestAnimationFrame js/window (loop (assoc state :time time) (inc frame))))))]
    ((loop state 0) 0)))


(defn main
  "entering point"
  []
  (let [state {:ui (brawlui/init)
               :world (world/init)
               :ui-drawer (uiwebgl/init)
               :world-drawer (webgl/init)
               :ui-ratio (min 2.0 (or (.-devicePixelRatio js/window) 1.0))
               :time 0
               :gametime 0
               :theme-started false
               :level 0
               :msgch (chan)
               :sounds (audio/sounds)
               :buffer (floatbuffer/create!)
               :metrics (metrics/basemetrics-random)
               :volumes {:music 0.5 :effects 0.5}
               :physics false
               :keycodes {}
               :controls {}
               :commands-ui []
               :commands-world []}

        final (-> state
                (defaults/load-defaults!)
                (audio/set-effects-volume)
                (audio/set-music-volume)
                (brawlui/load-ui layouts/info))]

    (load-font! final "Ubuntu Bold" "css/Ubuntu-Bold.ttf")
    (init-events! final)
    (resize-context!)
    (world/load-level! final (:level final))

    (animate
      final
      (fn [prestate frame time delta]
        (let [{gametime :gametime} prestate
              usedstate (assoc prestate :gametime (+ delta gametime))]
          (if-not (= (mod frame 1) 0) ;; frame skipping for development
            usedstate
            (-> usedstate
              (simulate time)
               ;; drawing
              (draw-world frame)
              (draw-ui))))))))

;; start main once, avoid firing new runloops with new reloads
(defonce mainloop (main))
