(ns ^:figwheel-hooks brawl.core
  (:require [goog.dom :as gdom]
            [tubax.core :refer [xml->clj]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [cljs.core.async :refer-macros [go]]
            [cljs-webgl.context :as context]
            [cljs-webgl.shaders :as shaders]
            [cljs-webgl.constants.draw-mode :as draw-mode]
            [cljs-webgl.constants.data-type :as data-type]
            [cljs-webgl.constants.buffer-object :as buffer-object]
            [cljs-webgl.constants.shader :as shader]
            [cljs-webgl.buffers :as buffers]
            [cljs-webgl.typed-arrays :as ta]
            [brawl.surface :as surface]
            [brawl.svg :as svg]
            [brawl.physics :as physics]
            [brawl.mass :as mass]
            [brawl.math4 :as math4]
            [brawl.shape :as shape]))
  
;;(println "AA This text is printed from src/brawl/core.cljs. Go ahead and edit it and see reloading in action.")


(def vertex-source
  "attribute highp vec4 position;
   attribute highp vec4 color;
   varying highp vec4 colorv;
   varying highp vec4 positionv;
   uniform mat4 projection;
   void main ( )
   {
	gl_Position = projection * position;
	gl_PointSize = 8.0;
	colorv = color;
	positionv = position;
   }")


(def fragment-source
  "varying highp vec4 colorv;
   varying highp vec4 positionv;
   void main( )
   {
	gl_FragColor = colorv;
   	if ( colorv.w == 1.0 && colorv.x < 0.8 )
   	{
       	 highp float ver = sin(positionv.y)+1.0;
       	 highp float hor = sin(positionv.x)+1.0;
       	 highp float dia = sin((positionv.x+positionv.y)/1.5)+1.0;
       	 ver = floor(ver * 2.0)/4.0;
       	 hor = floor(hor * 2.0)/4.0;
       	 dia = floor(dia * 2.0)/4.0;
       	 if ( colorv.x >= colorv.y && colorv.x >= colorv.z ) gl_FragColor.x -= ver*0.05;
       	 if ( colorv.y >= colorv.x && colorv.y >= colorv.z ) gl_FragColor.y -= hor*0.05;
	 if ( colorv.z >= colorv.x && colorv.z >= colorv.y ) gl_FragColor.z -= dia*0.2;
   	}
   }")


(def state (atom { :keypresses { } } ) )


(defn animate [draw-fn]
  (letfn [(loop [frame]
            (fn []
              (.requestAnimationFrame js/window (loop (inc frame)))
              (draw-fn frame)))]
    ((loop 0))))


(defn key-down-handler [event]
  (println "key-down" (.-keyCode event) )
  (swap! state assoc-in [:keypresses (.-keyCode event) ] true))


(defn key-up-handler [event]
  (println "key-up" (.-keyCode event ) )
  (swap! state assoc-in [:keypresses (.-keyCode event) ] false)
  (swap! state update-in [:trans 0] #(+ % 10.0))
  )


(defn start []
  (let
      [context (context/get-context (.getElementById js/document "main"))

       shader (shaders/create-program
               context
               (shaders/create-shader context shader/vertex-shader vertex-source)
               (shaders/create-shader context shader/fragment-shader fragment-source))

       scene_buffer (buffers/create-buffer
                      context
                      (ta/float32 (:vertexes @state))
                      ;;(ta/float32 [ 1.0  1.0 0.0 1.0 1.0 1.0 0.0 1.0
                      ;;             -1.0  1.0 0.0 1.0 1.0 0.0 0.0 1.0
                      ;;              1.0 -1.0 0.0 1.0 1.0 0.0 0.0 1.0])
                      buffer-object/array-buffer
                      buffer-object/static-draw)

       actor_buffer (buffers/create-buffer
                      context
                      (ta/float32 [ 500.0  500.0 0.0 1.0 1.0 1.0 1.0 1.0 ])
                      buffer-object/array-buffer
                      buffer-object/dynamic-draw)
       
       location_pos (shaders/get-attrib-location context shader "position")
       location_col (shaders/get-attrib-location context shader "color")

       projection (math4/proj_ortho 0.0 1500.0 1500.0 0.0 -1.0 1.0)]

    (set! (.-onkeydown js/document) key-down-handler)
    (set! (.-onkeyup js/document) key-up-handler)
    
    ;; runloop
    
    (animate
     (fn [frame]
           (buffers/clear-color-buffer context 0.1 0.0 0 1)

           ;; (actors/update actor controlstate)

           ;; draw scene buffer

           (.bindBuffer context buffer-object/array-buffer scene_buffer)
           
           (buffers/draw!
            context
            :count (/ (count (:vertexes @state)) 8)
            :shader shader
            :draw-mode draw-mode/triangles               
            :attributes [{:buffer scene_buffer
                          :location location_pos
                          :components-per-vertex 4
                          :type data-type/float
                          :offset 0
                          :stride 32}
                         {:buffer scene_buffer
                          :location location_col
                          :components-per-vertex 4
                          :type data-type/float
                          :offset 16
                          :stride 32}]
            :uniforms [{:name "projection"
                        :type :mat4
                        :values projection}])

       (let [center (ta/float32 (concat (:trans @state ) [0.0 1.0 1.0 1.0 1.0 1.0]))]

           ;; draw actor buffer

           (.bindBuffer context buffer-object/array-buffer actor_buffer)

           ;; load in new vertexdata

           (.bufferData context buffer-object/array-buffer center buffer-object/dynamic-draw)
                        
           (buffers/draw!
            context
            :count 1
            :shader shader
            :draw-mode draw-mode/points               
            :attributes [{:buffer actor_buffer
                          :location location_pos
                          :components-per-vertex 4
                          :type data-type/float
                          :offset 0
                          :stride 32}
                         {:buffer actor_buffer
                          :location location_col
                          :components-per-vertex 4
                          :type data-type/float
                          :offset 16
                          :stride 32}]
            :uniforms [{:name "projection"
                        :type :mat4
                        :values projection}]
            )
           )           
       )
     )
    )
  )


(defn gen-vertex-triangle [ vertexes color ]
  (let [r ( / (bit-shift-right (bit-and color 0xFF0000) 16) 255.0 )
        g ( / (bit-shift-right (bit-and color 0x00FF00) 8) 255.0 )
        b ( / (bit-and color 0x0000FF) 255.0 ) ]
    (map #( concat % [0.0 1.0 r g b 1.0] ) vertexes )))


(defn init[]
  (go
    (let [response (<! (http/get "level0.svg"
                                 ;; parameters
                                 {:with-credentials? false}))
          xmlstr (xml->clj (:body response) {:strict false})
          level (svg/psvg xmlstr "")
          shapes level ;; (filter #(not= (% :id) "Surfaces") level)
          surfacepoints (filter #(= (% :id) "Surfaces") level)
          surfaces (surface/generate-from-pointlist surfacepoints)
          masses [(mass/mass2 500.0 0.0)]
          triangles (flatten
                     ( map
                      (fn [shape]
                        (if (contains? shape :color)
                          ( gen-vertex-triangle (shape/triangulate_c (:path shape) ) (:color shape ))))
                      shapes))]
      ;;(for [x (range 0 10)] (p/mass2 (rand 1000) (rand 1000)))]

     
      {:mainmass (mass/mass2 500.0 0.0)
       :trans [0.0 0.0]
       :shapes shapes
       :masses masses
       :surfaces surfaces
       :surfacepoints surfacepoints}

      (swap! state assoc :vertexes triangles)
      (swap! state assoc :masses masses)
      (swap! state assoc :trans [500.0 500.0])
      
      (start)
      )
    ))

(init)

;; template functions

(defn multiply [a b] (* a b))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"}))

(defn get-app-element []
  (gdom/getElement "app"))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! app-state update-in [:__figwheel_counter] inc)
  (println "app-state" app-state)
)
