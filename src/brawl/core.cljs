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
            [cljs-webgl.typed-arrays :as ta] )
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(defn animate [draw-fn]
  (letfn [(loop [frame]
            (fn []
              (.requestAnimationFrame  js/window (loop (inc frame)))
              (draw-fn frame)))]
    ((loop 0))))

(def vertex-shader-source
  "attribute vec3 vertex_position;
   void main() {
     gl_Position = vec4(vertex_position, 1);
   }")

(def fragment-shader-source
  "uniform int frame;
   void main() {
     gl_FragColor.r = sin(float(frame) * 0.05) / 2.0 + 0.5;
     gl_FragColor.g = sin(float(frame) * 0.1) / 2.0 + 0.5;
     gl_FragColor.b = sin(float(frame) * 0.02) / 2.0 + 0.5;
     gl_FragColor.a = 1.0;
   }")


(defn start []
  (let
      [gl (context/get-context (.getElementById js/document "main"))
       
       shader (shaders/create-program gl
              (shaders/create-shader gl shader/vertex-shader vertex-shader-source)
              (shaders/create-shader gl shader/fragment-shader fragment-shader-source))

       vertex-buffer (buffers/create-buffer gl (ta/float32 [1.0 1.0 0.0
                                                            -1.0 1.0 0.0
                                                            1.0 -1.0 0.0])
                                            buffer-object/array-buffer
                                            buffer-object/static-draw)
       
       element-buffer (buffers/create-buffer gl (ta/unsigned-int16 [0 1 2])
                                             buffer-object/element-array-buffer
                                             buffer-object/static-draw)]

       (animate
        (fn [frame]
              
              (-> gl
                  (buffers/clear-color-buffer 1 0 0 1)
                  (buffers/draw! :shader shader
                                 :draw-mode draw-mode/triangles
                                 :count 3
                                 
                                 :attributes
                               [{:buffer vertex-buffer
                                 :location (shaders/get-attrib-location gl shader "vertex_position")
                                 :components-per-vertex 3
                                 :type data-type/float}]
                               
                               :uniforms
                               [{:name "frame" :type :int :values (ta/int32 [frame])}]
                               
                               :element-array
                               {:buffer element-buffer
                                :count 3
                                :type data-type/unsigned-short
                                :offset 0}))))))

(start)

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


;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "WellHello world!"}))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  (swap! app-state update-in [:__figwheel_counter] inc)
  (println "appsate" app-state)
)
