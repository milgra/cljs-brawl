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
            [brawl.shape :as shape]

            ))
  
;;(println "AA This text is printed from src/brawl/core.cljs. Go ahead and edit it and see reloading in action.")

(defn animate [draw-fn]
  (letfn [(loop [frame]
            (fn []
              (.requestAnimationFrame  js/window (loop (inc frame)))
              (draw-fn frame)))]
    ((loop 0))))

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

(defn get-test []
  (go
    (let [response (<! (http/get "level0.svg"
                                 ;; parameters
                                 {:with-credentials? false
                                  :query-params {"since" 135}}))]
      (prn  (:body response)))))


(defn post-test []
  (go
    (let [response (<! (http/post "https://api.github.com/graphql"
                                  {:with-credentials? false
                                   :headers {"Authorization" "SuperSecretToken1234"}
                                   :json-params {:query "query { viewer { login }}"}}))]
      (prn (:data (:body response))))))




(defn starttest []
  (println "starttest")
  (let
      [gl (context/get-context (.getElementById js/document "main"))
       shader (shaders/create-program
               gl
               (shaders/create-shader gl shader/vertex-shader vertex-source)
               (shaders/create-shader gl shader/fragment-shader fragment-source))

       vertex-buffer (buffers/create-buffer
                      gl
                      (ta/float32 [ 1.0  1.0 0.0 1.0 1.0 1.0 0.0 1.0
                                   -1.0  1.0 0.0 1.0 1.0 0.0 0.0 1.0
                                    1.0 -1.0 0.0 1.0 1.0 0.0 0.0 1.0])
                      buffer-object/array-buffer
                      buffer-object/static-draw)

       location_pos (shaders/get-attrib-location gl shader "position")
       location_col (shaders/get-attrib-location gl shader "color")

       projection (math4/proj_ortho -1.0 1.0 -1.0 1.0 -1.0 1.0)]

    
    (println "projection e" projection )
    
;;       (animate
;;        (fn [frame]              
          (-> gl
              (buffers/clear-color-buffer 0.1 0.0 0 1)
              
              (buffers/draw!
               :shader shader
               :draw-mode draw-mode/triangles
               :count 3
               
               :attributes
               [{:buffer vertex-buffer
                 :location location_pos
                 :components-per-vertex 4
                 :type data-type/float
                 :offset 0
                 :stride 32}
                {:buffer vertex-buffer
                 :location location_col
                 :components-per-vertex 4
                 :type data-type/float
                 :offset 16
                 :stride 32}]
               
               :uniforms
               [{:name "projection"
                 :type :mat4
                 :values projection}]
           
;;           )
;;          )
               )
              )
          )
  )

(defn gen-vertex-triangle [ vertexes color ]
  (let [r ( / (bit-shift-right (bit-and color 0xFF0000) 16) 255.0 )
        g ( / (bit-shift-right (bit-and color 0x00FF00) 8) 255.9 )
        b ( / (bit-and color 0x0000FF) 255.0 ) ]
    (println "vertex" r g b)
    [0.0 1.0 r g b]
    )
  )


(defn init[]
  (go
    (let [response (<! (http/get "level0.svg"
                                 ;; parameters
                                 {:with-credentials? false}))
          xmlstr (xml->clj (:body response) {:strict false})
          level (svg/psvg xmlstr "")
          shapes (filter #(not= (% :id) "Surfaces") level)
          surfacepoints (filter #(= (% :id) "Surfaces") level)
          surfaces (surface/generate-from-pointlist surfacepoints)
          masses [(mass/mass2 500.0 0.0)]
          triangles ( map (fn [shape] gen-vertex-triangle (shape/triangulate_c (:path shape) ) (:color shape ) ) shapes )]
      ;;(for [x (range 0 10)] (p/mass2 (rand 1000) (rand 1000)))]
      
      (println "shapes " shapes)
      (println "triangles " triangles)

      {:mainmass (mass/mass2 500.0 0.0)
       :trans [0.0 0.0]
       :keysdown { :l-down false :r-down false :u-down false :d-down false }
       :shapes shapes
       :masses masses
       :surfaces surfaces
       :surfacepoints surfacepoints})

    (starttest)
    ))

(init)

(defn multiply [a b] (* a b))


;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"}))

(defn get-app-element []
  (gdom/getElement "app"))



;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
