(ns ^:figwheel-hooks brawl.core
  (:require
   [cljs.core.async :refer [<! chan put! take! poll!]]
   [goog.dom :as dom]
   [goog.events :as events]
   [gui.math4 :as math4]
   [gui.webgl :as uiwebgl]
   [brawl.ui :as brawlui]
   [brawl.world :as world]
   [brawl.webgl :as webgl]
   [brawl.audio :as audio]
   [brawl.layouts :as layouts]
   [brawl.particle :as particle]
   [brawl.defaults :as defaults]
   [brawl.actorskin :as actorskin]
   [brawl.floatbuffer :as floatbuffer])
  (:import [goog.events EventType]))


(defn resize-context!
  "vresize canvas on window resize"
  []
  (dom/setProperties
   (dom/getElement "main")
   (clj->js {:width (.-innerWidth js/window)
             :height (.-innerHeight js/window)})))


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
     EventType.TOUCHSTART
     (fn [event] nil))

    (events/listen
     js/document
     EventType.MOUSEDOWN
     (fn [event]
       (swap! mouse-down not)
       (put! (:msgch state) {:id "mouse" :type "down" :point [(.-clientX event) (.-clientY event)]})))

    (events/listen
     js/document
     EventType.MOUSEUP
     (fn [event]
       (swap! mouse-down not)
       (put! (:msgch state) {:id "mouse" :type "up" :point [(.-clientX event) (.-clientY event)]})))

    (events/listen
     js/document
     EventType.MOUSEMOVE
     (fn [event]
       (if @mouse-down (put! (:msgch state) {:id "mouse" :type "down" :point [(.-clientX event) (.-clientY event)]}))))

    (events/listen
     js/window
     EventType.RESIZE
     (fn [event]
       (put! (:msgch state) {:id "resize"})
       (resize-context!)))))


(defn draw-ui
  "draw ui"
  [{{:keys [views viewids] :as ui} :ui
    ui-drawer :ui-drawer :as state}
   frame]
  (let [projection (math4/proj_ortho 0 (.-innerWidth js/window) (.-innerHeight js/window) 0 -10.0 10.0)
        new-drawer (uiwebgl/draw! ui-drawer projection (map views viewids))]
    (assoc state :ui-drawer new-drawer)))


(defn calc-view-rect [actor]
  (let [[fax fay] (:p (get-in actor [:bases :base_l]))
        [fbx fby] (:p (get-in actor [:bases :base_r]))
        [tx ty] [ (+ fax (/ (- fbx fax ) 2)) (+ fay (/ (- fby fay) 2))  ]
        ratio 1.0
        r (/ (.-innerWidth js/window) (.-innerHeight js/window) )
        h (* 350.0 ratio)
        w (* h r)]
    [(- tx w) (+ tx w) (+ ty h) (- ty h)]))


(defn draw-world
  "draws background, actors, masses with projection"
  [{:keys [game-drawer trans buffer]
    {:keys [actors particles surfacelines] :as world} :world
    level :level :as state}
   frame
   msg]
  (if (:inited world)
    (let [[l r b t :as view-rect] (calc-view-rect (:hero actors))
          projection (math4/proj_ortho (+ l 50) (- r 50) (- b 50) (+ t 50) -1.0 1.0)
          variation (Math/floor (mod (/ frame 10.0) 3.0 ))
          newbuf (floatbuffer/empty! buffer)
          newbuf1 (reduce (fn [oldbuf [id actor]] (actorskin/get-skin-triangles actor oldbuf variation view-rect)) newbuf actors)]
      ;; draw triangles
      (webgl/clear! game-drawer)
      (webgl/drawshapes! game-drawer projection trans variation)
      (webgl/drawtriangles! game-drawer projection newbuf1)

      (let [newbuf2 (floatbuffer/empty! newbuf1)
            newbuf3 (reduce (fn [oldbuf particle] (particle/get-point particle oldbuf)) newbuf2 particles) ;; particles
            newbuf4 (reduce (fn [oldbuf [id actor]] (actorskin/getpoints actor oldbuf view-rect)) newbuf3 actors)]

        ;; draw points
        (webgl/drawpoints! game-drawer projection newbuf4)

        (let [newbuf5 (floatbuffer/empty! newbuf4)
              newbuf6 (reduce (fn [oldbuf [id actor]] (actorskin/getlines actor oldbuf view-rect)) newbuf5 actors)
              newbuf7 (floatbuffer/append! newbuf6 surfacelines)]
          ;; draw lines
          (webgl/drawlines! game-drawer projection newbuf7)

          (-> state
              (assoc :game-drawer game-drawer)
              (assoc-in [:world :view-rect] view-rect)
              (assoc :buffer newbuf7)))))
    state))


(defn update-controls
  "set up control state based on keycodes"
  [{:keys [keycodes controls] :as state } msg]
  (if (and msg (= (:id msg) "key"))
    (let [new-codes (assoc (:keycodes state) (:code msg) (:value msg))
          new-controls {:left (new-codes 37)
                        :right (new-codes 39)
                        :up (new-codes 38)
                        :down (new-codes 40)
                        :punch (new-codes 70)
                        :run (new-codes 32)
                        :kick (new-codes 83)
                        :block (new-codes 68)}]
      (-> state
          (assoc :keycodes new-codes)
          (assoc :controls new-controls)))
    state))


(defn animate
  "main runloop, syncs animation to display refresh rate"
  [state draw-fn]
  (letfn [(loop [prestate frame]
            (fn [time]
              (let [newstate (if (> time 0)
                               (draw-fn prestate frame time)
                               prestate)]
                (.requestAnimationFrame
                 js/window
                 (loop newstate (inc frame))))))]
    ((loop state 0) 0 )))


(defn main
  "entering point"
  []
  
  (resize-context!)

  (let [state {:ui (brawlui/init)
               :world (world/init)
               :ui-drawer (uiwebgl/init)
               :game-drawer (webgl/init)
               :level 0
               :msgch (chan)
               :sounds (audio/sounds)
               :buffer (floatbuffer/create!)
               :volumes {:music 0.5 :effects 0.5}
               :keycodes {}
               :controls {}
               :commands-ui []
               :commands-world []}
        
        final (-> state
                  (defaults/load-defaults!)
                  (brawlui/load-ui layouts/info))]

    (load-font! final "Ubuntu Bold" "css/Ubuntu-Bold.ttf")
    (init-events! final)
    (world/load-level! final (:level final))

    (animate
     final
     (fn [prestate frame time]
       (if (= (mod frame 1) 0 ) ; frame skipping for development
         (let [msg (poll! (:msgch prestate))]
           (-> prestate
               ;; get controls
               (update-controls msg)
               ;; world
               (world/execute-commands)
               (world/reset-world msg)
               (world/update-world msg)
               ;; ui
               (brawlui/execute-commands)
               (brawlui/update-ui msg)
               ;; drawing
               (draw-world frame msg)
               (draw-ui frame)))
         prestate)))))

(defonce mainloop (main))
