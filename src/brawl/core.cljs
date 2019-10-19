(ns brawl.core
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [cljs-webgl.context :as context]
            [cljs-webgl.shaders :as shaders]
            [cljs-webgl.constants.draw-mode :as draw-mode]
            [cljs-webgl.constants.data-type :as data-type]
            [cljs-webgl.constants.buffer-object :as buffer-object]
            [cljs-webgl.constants.shader :as shader]
            [cljs-webgl.buffers :as buffers]
            [cljs-webgl.typed-arrays :as ta]
            [clojure.data.xml :as xml]
            [brawl.surface :as surface]
            [brawl.svg :as svg]
            [brawl.physics :as physics]
            [brawl.mass :as mass])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

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


(defn default_ortho [ left right bottom top near far ]
  (let [rpl ( + right left )
        rml ( - right left )
        tpb ( + top bottom )
        tmb ( - top bottom )
        fpn ( + far near )
        fmn ( - far near ) ]

    [( / 2.0 rml )
     0.0
     0.0
     0.0
     0.0
     ( / 2.0 tmb )
     0.0
     0.0
     0.0
     0.0
     ( / -2.0 fmn )
     0.0
     (/ (- rpl) rml)
     (/ (- tpb) tmb)
     (/ (- fpn) fmn)
     1.0 ]))

(defn init[]
(go
  (let [response (<! (http/get "level0.svg"
                           ;; parameters
                           {:with-credentials? false}))

        xmlstr (xml/parse-str (:body response))
        level (svg/psvg xmlstr "")]
        ;;shapes (filter #(not= (% :id) "Surfaces") level)
        ;;surfacepoints (filter #(= (% :id) "Surfaces") level)
        ;;surfaces (surface/generate-from-pointlist surfacepoints)
        ;;masses [(mass/mass2 500.0 0.0)]]
        ;;(for [x (range 0 10)] (p/mass2 (rand 1000) (rand 1000)))

    
    (println "level" level))))
    
   ;; {:mainmass (mass/mass2 500.0 0.0)
   ;;  :trans [0.0 0.0]
   ;;  :keysdown { :l-down false :r-down false :u-down false :d-down false }
   ;;  :shapes shapes
   ;;  :masses masses
   ;;  :surfaces surfaces
   ;;  :surfacepoints surfacepoints})))


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
  (init)
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

       projection (default_ortho -1.0 1.0 -1.0 1.0 -1.0 1.0)]

    
    (println "projection" projection)
    
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
`                 :stride 32}
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


(starttest)


;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "WellHello world!"}))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! app-state update-in [:__figwheel_counter] inc)
  (println "appsate" app-state)
)
