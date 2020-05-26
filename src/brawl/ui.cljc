(ns brawl.ui
  (:require [gui.kinetix :as kinetix]
            [gui.webgl :as uiwebgl]
            [gui.math4 :as math4]))


(defn init []
  {:webgl (uiwebgl/init)
   :views {}
   :baseid nil 
   :viewids []})


(defn load-ui
  "load new ui stack"
  [{{:keys [webgl views viewids] :as ui} :ui :as state }  description]
  (let [views (kinetix/gen-from-desc {} description)
        baseid (keyword (:id description))
        viewids (kinetix/collect-visible-ids views [baseid] "")
        alignedviews (kinetix/align views [baseid] 0 0 (. js/window -innerWidth) (. js/window -innerHeight))]

    (assoc state :ui {:webgl webgl
                      :baseid baseid
                      :views alignedviews
                      :viewids viewids})))


(defn draw-ui [{{:keys [webgl views viewids] :as ui} :ui :as state} frame]
  (let [projection (math4/proj_ortho 0 (.-innerWidth js/window) (.-innerHeight js/window) 0 -10.0 10.0)
        newwebgl (uiwebgl/draw! webgl projection (map views viewids))]
    (assoc-in state [:ui :webgl] newwebgl)))

  
(defn update-ui [{{:keys [baseid views commands webgl]} :ui :as state} msg]
  (let [results (if (and msg (= (:id msg) "mouse"))
                  (let [touched-views (kinetix/collect-pressed-views views (:point msg))]
                    (reduce
                     (fn [result {:keys [class] :as view}]
                       (cond
                         (= class "Slider") (conj result (kinetix/touch-slider view views msg))
                         (= class "Button") (conj result (kinetix/touch-button view views msg))
                         :else result))
                     []
                     (map views touched-views)))
                  [])
        newviews (reduce (fn [oldviews {newviews :views}]
                           (reduce #(assoc oldviews (:id %2) %2) oldviews newviews))
                         views
                         results)
        
        newnewviews (if (and msg (= (:id msg) "resize"))
                      (kinetix/align newviews [baseid] 0 0 (. js/window -innerWidth) (. js/window -innerHeight))
                      newviews)
        
        newcommands (map :command results)]
    (cond-> state
      true (assoc-in [:ui :views] newnewviews)
      true (assoc :commands (concat commands newcommands))
      (and msg (= (:id msg) "redraw-ui")) (assoc-in [:ui :webgl] (uiwebgl/reset webgl)))))


(defn align [{{:keys [baseid views commands webgl]} :ui :as state}]
  (assoc-in state [:ui :views] (kinetix/align views [baseid] 0 0 (. js/window -innerWidth) (. js/window -innerHeight))))


(defn update-gen-sliders [{:keys [ui world] :as state}]
  (let [views (:views ui)
        {:keys [height hitpower hitrate stamina speed]} (get-in world [:actors :hero :metrics :base])
        hpsl (:Hitpower views)
        hrsl (:Hitrate views)
        hesl (:Height views)
        spsl (:Speed views)
        stsl (:Stamina views)
        newv (-> views
                 (kinetix/set-slider-value hpsl hitpower)
                 (kinetix/set-slider-value hrsl hitrate)
                 (kinetix/set-slider-value hesl height)
                 (kinetix/set-slider-value spsl speed)
                 (kinetix/set-slider-value stsl stamina))]
    (assoc-in state [:ui :views] newv)))


(defn set-slider-value [{ {:keys [views] } :ui :as state} id value]
  (assoc-in state [:ui :views] (kinetix/set-slider-value views (id views) value)))
