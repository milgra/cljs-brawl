(ns ^:figwheel-hooks brawl.core
  (:require [goog.dom :as gdom]
            [goog.events :as events]
            [tubax.core :refer [xml->clj]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! chan put! take! poll!]]
            [cljs.core.async :refer-macros [go]]
            [brawl.surface :as surface]
            [brawl.svg :as svg]
            [brawl.mass :as mass]
            [brawl.math4 :as math4]
            [brawl.shape :as shape]
            [brawl.webglo :as webglo]
            [brawl.actor :as actor]
            [brawl.ui :as ui])
  (:import [goog.events EventType]))
  

(defn load-level! [channel name]
  "load level svg's"
  (go
    (let [response (<! (http/get name
                                 {:with-credentials? false}))
          xmlstr (xml->clj (:body response) {:strict false})
          shapes (svg/psvg xmlstr "")]
      (put! channel shapes))))


(defn load-image! [channel name]
  "load touchscreen button images"
  (let [img (js/Image.)]
    (set! (.-onload img)
          (fn [a]
            (put! channel img)))
    (set! (.-src img) name)))


(defn resize-context! [ ]
  "resize canvas on window resize"
  (let [canvas (. js/document getElementById "main")]
        (set! (. canvas -width) (. js/window -innerWidth))
        (set! (. canvas -height) (. js/window -innerHeight))))


(defn init-events! [keych tchch]
  "start event listening"
 
  (events/listen
   js/document
   EventType.KEYDOWN
   (fn [event] (put! keych {:code (.-keyCode event) :value true})))
  
  (events/listen
   js/document
   EventType.KEYUP
   (fn [event] (put! keych {:code (.-keyCode event) :value false})))
  
  (events/listen
   js/window
   EventType.RESIZE
   (fn [event] (resize-context!))))


(defn animate [state draw-fn]
  "main runloop, syncs animation to display refresh rate"
  (letfn [(loop [prestate frame]
            (fn [time]
              (let [newstate (if (> time 0)
                               (draw-fn prestate frame time)
                               prestate)]
                (.requestAnimationFrame
                 js/window
                 (loop newstate (inc frame))))))]
    ((loop state 0) 0 )))


(defn main []
  "entering point"
  (let
      [world {:actors [(actor/init 480.0 300.0)]
              :masses [(mass/mass2 500.0 300.0 1.0 1.0 1.0)]
              :dguards []
              :aguards []
              :surfaces []
              :surfacelines []}

       state {:world world
              :glstate (webglo/init)
              :level_file "level0.svg"
              :level_state "none"
              :texfile "font.png"
              :keypresses {}
              :trans [500.0 300.0]
              :speed [0.0 0.0]}
       
       svgch (chan) ;; level svg channel
       imgch (chan) ;; texture image channel
       tchch (chan) ;; touch channel
       keych (chan)];; key press channel

    (resize-context!)
    (init-events! keych tchch)
    (load-image! imgch (:texfile state))
    
    ;; runloop
    
    (animate
     state
     (fn [prestate frame time]
       (cond 
         
         (= (:level_state prestate) "none")
         (do
           (load-level! svgch (:level_file prestate))
           (assoc prestate :level_state "loading"))
         
         (= (:level_state prestate) "loading")
         (let [shapes (poll! svgch)]
           (if shapes
             (let [surfacepts (filter #(and (= (% :id) "Surfaces") (not (contains? % :color))) shapes )
                   lines (partition 2 (flatten (map (fn [shape]
                                (partition 2 (flatten (partition 2 1 (:path shape)))))
                              surfacepts)))]
 
               (-> prestate
                   (assoc :glstate (webglo/loadshapes (:glstate prestate) shapes))
                   (assoc-in [:world :surfaces] (surface/generate-from-pointlist surfacepts))
                   (assoc-in [:world :surfacelines] lines )
                   (assoc :level_state "loaded")))
               prestate))

         (= (:level_state prestate) "loaded")
         (let [[tx ty] (:trans prestate)
               [sx sy] (:speed prestate)
               ratio (/ (min (max (Math/abs sx) (Math/abs sy)) 40.0) 40.0)
               r (/ (.-innerWidth js/window) (.-innerHeight js/window) )
               h 300.0
               w (* h r)
               projection (math4/proj_ortho
                           (- tx (+ w (* ratio 50.0)))
                           (+ tx (+ w (* ratio 50.0)))
                           (+ ty (+ h (* ratio 50.0)))
                           (- ty (+ h (* ratio 50.0)))
                           -1.0 1.0)

               teximage (poll! imgch)
               keyevent (poll! keych)
               tchevent (poll! tchch)

               variation (Math/floor (mod (/ frame 20.0) 3.0 ))

               preworld (:world prestate)
               surfaces (:surfaces preworld)
               masses (:masses preworld)

               newactor (actor/newstate (first (:actors preworld)) surfaces 1.0)
               
               newmasses (mass/update-masses masses surfaces 1.0)

               newglstate (if teximage
                            (webglo/loadtexture! (:glstate prestate) teximage)
                            (:glstate prestate))

               newstate (-> prestate
                            (assoc :glstate newglstate)
                            (assoc-in [:world :actors 0] newactor)
                            (assoc-in [:world :masses] newmasses))]
           
           ;; draw scene
           (webglo/drawshapes! (:glstate prestate) projection (:trans prestate) variation)
           (webglo/drawtriangles! (:glstate prestate) projection (actor/get-skin-triangles newactor))
           (webglo/drawlines! (:glstate prestate) projection (:surfacelines preworld))
           (webglo/drawpoints! (:glstate prestate) projection (map :trans newmasses))
           (webglo/drawpoints! (:glstate prestate) projection (actor/getpoints newactor))
           (webglo/drawlines! (:glstate prestate) projection (actor/getlines newactor))

           ;;(webglo/draw-ui-quads! (:glstate state) projection)
           ;; (actors/update actor controlstate)
           
           ;; handle keypresses, modify main point trans
           
           (let [keycodes (if keyevent
                            (assoc (:keypresses prestate) (:code keyevent) (:value keyevent))
                            (:keypresses prestate))

                 nsx (cond
                       (keycodes 37) (- sx 0.4)
                       (keycodes 39) (+ sx 0.4)
                       "default" (* sx 0.9))
                 
                 nsy (cond
                       (keycodes 38) (- sy 0.4)
                       (keycodes 40) (+ sy 0.4)
                       "default" (* sy 0.9))
                 
                 ntx (+ tx sx)
                 nty (+ ty sy)]
             
             ;; return with updated state
             (-> newstate
                 (assoc :keypresses keycodes)
                 (assoc :speed [nsx nsy])
                 (assoc :trans [ntx nty]))
             )))))))

(main)
