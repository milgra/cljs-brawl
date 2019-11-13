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
            [brawl.webgl :as webgl]
            [brawl.actor :as actor]
            [brawl.ui :as ui])
  (:import [goog.events EventType]))
  

(defn load-level! [channel name]
  (go
    (let [response (<! (http/get name
                                 {:with-credentials? false}))
          xmlstr (xml->clj (:body response) {:strict false})
          shapes (svg/psvg xmlstr "")]
      (put! channel shapes))))


(defn load-image! [channel name]
  (let [img (js/Image.)]
    (set! (.-onload img)
          (fn [a]
            (put! channel img)))
    (set! (.-src img) name)))


(defn animate [state draw-fn]
  (letfn [(loop [oldstate frame]
            (fn [time]
              (let [newstate (draw-fn oldstate frame time)]
              (.requestAnimationFrame js/window (loop newstate (inc frame))))
              ))]
    ((loop state 0) 0 )))


(defn resize-context! [ ]
  (let [canvas (. js/document getElementById "main")]
        (set! (. canvas -width) (. js/window -innerWidth))
        (set! (. canvas -height) (. js/window -innerHeight))))


(defn main []

  (let
      [initstate {:glstate (webgl/init)
                  :level_file "level0.svg"
                  :level_state "none"
                  :font-file "font.png"
                  :keypresses {}
                  :trans [500.0 300.0]
                  :speed [0.0 0.0]
                  :masses [(mass/mass2 500.0 300.0 1.0 1.0 1.0)]
                  :actor (actor/init 480.0 300.0)}

       filechannel (chan)
       keychannel (chan)
       imagechannel (chan)
       
       ]

    ;; key listeners

    (events/listen
     js/document
     EventType.KEYDOWN
     (fn [event] (put! keychannel {:code (.-keyCode event) :value true})))

    (events/listen
     js/document
     EventType.KEYUP
     (fn [event] (put! keychannel {:code (.-keyCode event) :value false})))

    (events/listen
     js/window
     EventType.RESIZE
     (fn [event]
       (resize-context!)))

    (resize-context!)

    (load-image! imagechannel (:font-file initstate))
    
    ;; runloop
    
    (animate
     initstate
     (fn [state frame time]
       (cond 
         
         (= (:level_state state) "none")
         (do
           (load-level! filechannel (:level_file state))
           (assoc state :level_state "loading"))
         
         (= (:level_state state) "loading")
         (let [shapes (poll! filechannel)]
           (if shapes
             (let [surfacepts (filter #(and (= (% :id) "Surfaces") (not (contains? % :color))) shapes )
                   lines (partition 2 (flatten (map (fn [shape]
                                (partition 2 (flatten (partition 2 1 (:path shape)))))
                              surfacepts)))]
 
               (-> state
                   (assoc :glstate (webgl/loadshapes (:glstate state) shapes))
                   (assoc :surfaces (surface/generate-from-pointlist surfacepts))
                   (assoc :lines lines )
                   (assoc :level_state "loaded")))
               state))


         (= (:level_state state) "loaded")
         (let [[tx ty] (:trans state)
               [sx sy] (:speed state)
               ratio (/ (min (max (Math/abs sx) (Math/abs sy)) 40.0) 40.0)
               r (/ (.-innerWidth js/window) (.-innerHeight js/window) )
               h 300.0
               w (* h r)
               projection (math4/proj_ortho
                           ;; (- tx 500.0)
                           ;; (+ tx 500.0)
                           ;; (+ ty 500.0)
                           ;; (- ty 500.0)
                           (- tx (+ w (* ratio 50.0)))
                           (+ tx (+ w (* ratio 50.0)))
                           (+ ty (+ h (* ratio 50.0)))
                           (- ty (+ h (* ratio 50.0)))
                           -1.0 1.0)

               image (poll! imagechannel)
               
               keyevent (poll! keychannel)

               variation (Math/floor (mod (/ time 500.0) 3.0 ))
               
               surfaces (:surfaces state)
               masses (:masses state)

               newactor (actor/newstate ( :actor state) surfaces 1.0)
               
               newmasses (mass/update-masses masses surfaces 1.0)

               newglstate (if image
                            (webgl/loadtexture! (:glstate state) image)
                            (:glstate state))

               newstate (-> state
                            (assoc :glstate newglstate)
                            (assoc :actor newactor)
                            (assoc :masses newmasses))]
           
           ;; draw scene
           (webgl/drawshapes! (:glstate state) projection (:trans state) variation)
           (webgl/drawtriangles! (:glstate state) projection (actor/get-skin-triangles newactor))
           (webgl/drawlines! (:glstate state) projection (:lines state))
           (webgl/drawpoints! (:glstate state) projection (map :trans newmasses))
           (webgl/drawpoints! (:glstate state) projection (actor/getpoints newactor))
           (webgl/drawlines! (:glstate state) projection (actor/getlines newactor))

           (webgl/draw-ui-quads! (:glstate state) projection)
           ;; (actors/update actor controlstate)
           
           ;; handle keypresses, modify main point trans
           
           (let [keycodes (if keyevent
                            (assoc (:keypresses state) (:code keyevent) (:value keyevent))
                            (:keypresses state))
                 
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
             )
           )
         )
       )
     )
    )
  )

;; template functions

;;(println "AA This text is printed from src/brawl/core.cljs. Go ahead and edit it and see reloading in action.")

(def app-state (atom {}))

(defn multiply [a b] (* a b))

(defn get-app-element []
  (gdom/getElement "app"))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! app-state update-in [:__figwheel_counter] inc)
  (println "app-state" app-state)
)

;; start entry point, can we do this from project.clj?
(main)
