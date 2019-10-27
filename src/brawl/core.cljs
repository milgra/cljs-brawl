(ns ^:figwheel-hooks brawl.core
  (:require [goog.dom :as gdom]
            [goog.events :as events]
            [tubax.core :refer [xml->clj]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<! chan put! take! poll!]]
            [cljs.core.async :refer-macros [go]]
            [brawl.surface :as surface]
            [brawl.svg :as svg]
            [brawl.physics :as physics]
            [brawl.mass :as mass]
            [brawl.math4 :as math4]
            [brawl.shape :as shape]
            [brawl.webgl :as webgl])
  (:import [goog.events EventType]))
  

(defn load-level! [channel name]
  (go
    (let [response (<! (http/get name
                                 {:with-credentials? false}))
          xmlstr (xml->clj (:body response) {:strict false})
          shapes (svg/psvg xmlstr "")]
      (put! channel shapes))))


(defn animate [state draw-fn]
  (letfn [(loop [oldstate frame]
            (fn [time]
              (let [newstate (draw-fn oldstate frame time)]
              (.requestAnimationFrame js/window (loop newstate (inc frame))))
              ))]
    ((loop state 0) 0 )))


(defn main []

  (let
      [initstate {:glstate (webgl/init)
                  :level_file "level0.svg"
                  :level_state "none"
                  :keypresses {}
                  :trans [500.0 500.0]
                  :speed [0.0 0.0]
                  :masses [(mass/mass2 500.0 400.0)]}

       filechannel (chan)
       keychannel (chan)]

    ;; key listeners

    (events/listen
     js/document
     EventType.KEYDOWN
     (fn [event] (put! keychannel {:code (.-keyCode event) :value true})))
    
    (events/listen
     js/document
     EventType.KEYUP
     (fn [event] (put! keychannel {:code (.-keyCode event) :value false})))

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
             (-> state
                 (assoc :glstate (webgl/loadshapes (:glstate state) shapes))
                 (assoc :surfaces (surface/generate-from-pointlist (filter #(= (% :id) "Surfaces") shapes)))
                 ;;shapes level ;; (filter #(not= (% :id) "Surfaces") level)
                 (assoc :level_state "loaded"))
             state))
         
         (= (:level_state state) "loaded")
         (let [[tx ty] (:trans state)
               [sx sy] (:speed state)
               ratio (/ (min (max (Math/abs sx) (Math/abs sy)) 40.0) 40.0)
               projection (math4/proj_ortho
                           (- tx 500.0)
                           (+ tx 500.0)
                           (+ ty 500.0)
                           (- ty 500.0)
                           ;; (- tx (+ 150.0 (* ratio 50.0)))
                           ;; (+ tx (+ 150.0 (* ratio 50.0)))
                           ;; (+ ty (+ 150.0 (* ratio 50.0)))
                           ;; (- ty (+ 150.0 (* ratio 50.0)))
                           -1.0 1.0)
               
               keyevent (poll! keychannel)

               variation (Math/floor (mod (/ time 500.0) 3.0 ))
               
               surfaces (:surfaces state)
               masses (:masses state)
               
               newmasses (mass/update-masses masses surfaces 1.0)]

           ;; draw scene
           
           (webgl/drawshapes! (:glstate state) projection (:trans state) variation)
           (webgl/drawlines! (:glstate state) projection )
           (webgl/drawmasses! (:glstate state) projection newmasses)
           
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
             
             (-> state
                 (assoc :masses newmasses)
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
