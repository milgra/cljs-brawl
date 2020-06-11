(ns gui.kinetix
  (:require [gui.texmap :as texmap]
            [gui.bitmap :as bitmap]
            [gui.webgl :as webgl]
            [clojure.string :as str]))
  
;; individual views

(defn create-view [id class width height texture]
  "generate basic view with color"
  {:x 0
   :y 0
   :id id
   :class class
   :width width
   :height height
   :texture texture ; "Color 0xFFFFFFFF" "Label Text 0xFFFFFFFF 0x000000FF" "Debug" 
   :subviews []})

  
(defn gen-id! [length]
  "generates fixed length alfanumeric hash"
   (let [chars (map char (concat (range 48 57) (range 65 90) (range 97 122)))
         id (take length (repeatedly #(rand-nth chars)))]
     (reduce str id)))


(defn add-align [view top bottom left right center-x center-y]
  "add alignment properties to view"
  (-> view
      (assoc :top top)
      (assoc :bottom bottom)
      (assoc :left left)
      (assoc :right right)
      (assoc :center-x center-x)
      (assoc :center-y center-y)))


(defn add-subview [{subviews :subviews :as view} subview]
  "inserts subview's id to views subviews property"
  (assoc view :subviews (conj subviews (subview :id))))

;; generator

(defn setup-view [{:keys [class] :as view}]
  "setup view and create subviews if needed"
  (cond
    (= class "Slider")
    (let [sldview (create-view (gen-id! 5) "None" {:pixel 10.0} {:ratio 1.0} {:type "Color" :color 0x009900FF})
          lblview (create-view (gen-id! 5) "Label" {:ratio 1.0} {:ratio 1.0} (:label view))
          newview (-> view
                      (add-subview sldview)
                      (add-subview lblview))]
      [newview sldview lblview]) ; return the modified view and the new view
    (= class "Button")
    (let [indview (create-view (gen-id! 5) "None" {:pixel 0.0} {:ratio 1.0} {:type "Color" :color 0xFF0000FF})
          lblview (create-view (gen-id! 5) "Label" {:ratio 1.0} {:ratio 1.0} (:label view))
          newview (-> view
                      (add-subview indview)
                      (add-subview lblview))]
      [newview indview lblview]) ; return the modified view and the new view
    :else [view])) ; return the view only


(defn get-value [text scale]
  "extract measurement, value and element from alignment/dimension value"
  (let [[element value measure] (str/split text #" ")]
    (if (= measure nil)
      (if (= value "px") ; two element value for width and height , "100 px" or "50 %"
        {:pixel (int (* (js/parseInt element) scale))}
        {:ratio (/ (js/parseInt element) 100)})
      (let [elem (if (= element "Edge") nil (keyword element))]
        (if (= measure "px") ; three element value for alignments , "Edge 50 %" or "Button 5 px"
          {:element elem :pixel (int (* (js/parseInt value) scale))}
          {:element elem :ratio (/ (js/parseInt value) 100.0)})))))


(defn gen-from-desc [viewmap viewdesc scale]
  "generate view structure from description"
  (let [subviews   (:subviews viewdesc)
        subids     (if subviews (map (fn [desc] (keyword (:id desc))) subviews) []) ; finalsubviews property needs ids only
        subviewmap (if subviews (reduce (fn [oldmap desc] (gen-from-desc oldmap desc scale)) viewmap subviews) viewmap) ; generate subviews into viewmap
        view (reduce
              (fn [result [k v]]
                (case k
                  :id       (assoc result :id (keyword v))
                  :class    (assoc result :class v)
                  :command  (assoc result :command v)
                  :color    (assoc result :color v)
                  :width    (assoc result :width (get-value v scale))
                  :height   (assoc result :height (get-value v scale))
                  :center-x (assoc result :center-x (get-value v scale))      
                  :center-y (assoc result :center-y (get-value v scale))
                  :left     (assoc result :left (get-value v scale))
                  :right    (assoc result :right (get-value v scale))
                  :top      (assoc result :top (get-value v scale))
                  :bottom   (assoc result :bottom (get-value v scale))
                  :subviews (assoc result :subviews subids)
                  :label    (assoc result :label (update v :size * scale))
                  (assoc result k v))) {:x 0.0 :y 0.0 :w 150.0 :h 50.0 :subviews []} viewdesc)
    newviews (setup-view view)] ; generate subviews for sliders, buttons, etc
  (reduce #(assoc %1 (:id %2) %2) subviewmap newviews)))

;; alignment

(defn align-view [viewmap id px py pwidth pheight]
  "aligns view"
  (let [{:keys [x y w h width height top bottom left right center-x center-y] :as view } (get viewmap id)
        neww (cond
               (:pixel width) (:pixel width)
               (:ratio width) (* (:ratio width) pwidth)
               :else w)

        newh (cond
               (:pixel height) (:pixel height)
               (:ratio height) (* (:ratio height) pheight)
               :else h)

        newx (cond
               (:ratio center-x) (- (+ px (* pwidth (:ratio center-x))) (* neww 0.5)) ; align view center to ratio
               (:pixel center-x) (- (+ px (:pixel center-x)) (* neww 0.5)) ; align view center by pixel
               (:element left) (+ (:x ((:element left) viewmap)) (:w ((:element left) viewmap)) (:pixel left)) ; align view to pixel distance from top view
               (:element right) (- (:x ((:element right) viewmap)) (:pixel right) neww) ; align view to pixel distance from bottom view
               (:pixel left) (+ px (:pixel left)) ; align view to the left based on ratio
               (:pixel right) (- (+ px pwidth) (:pixel right) neww) ; align view to the right based on ratio
               (:ratio left) (+ px (* (:ratio left) pwidth)) ; align view to the left based on ratio
               (:ratio right) (- (+ px (* (:ratio right) pwidth)) neww) ; align view to the right based on ratio
               :else px)
               
        newy (cond
               (:ratio center-y) (- (+ py (* pheight (:ratio center-y))) (* newh 0.5)) ; align view center to ratio
               (:pixel center-y) (- (+ py (:pixel center-y)) (* newh 0.5)) ; align view center by pixel
               (:element top) (+ (:y ((:element top) viewmap)) (:h ((:element top) viewmap)) (:pixel top)) ; align view to pixel distance from top view
               (:element bottom) (- (:y ((:element bottom) viewmap)) (:pixel bottom) newh) ; align view to pixel distance from bottom view
               (:pixel top) (+ py (:pixel top)) ; align view to the left based on ratio
               (:pixel bottom) (- (+ py pheight) (:pixel bottom) newh) ; align view to the right based on ratio
               (:ratio top) (+ py (* (:ratio top) pheight)) ; align view to the left based on ratio
               (:ratio bottom) (- (+ py (* (:ratio bottom) pheight)) newh) ; align view to the right based on ratio
               :else py)]
    
    (assoc view :w neww :h newh :x newx :y newy)))


(defn align [viewmap coll px py pwidth pheight]
  "iterate through all views and align them based on their alignment switches"
  (reduce (fn [result id]
            (let [{:keys [top bottom left right center-x center-y] :as view} (get result id)
                  related-views (remove nil? (map :element [top bottom left right center-x center-y])) ; get views to align before actual view
                  related-viewmap (align result related-views px py pwidth pheight) ; first align relative views
                  aligned-view (align-view related-viewmap id px py pwidth pheight) ; align self
                  {:keys [x y w h subviews]} aligned-view  ; align self
                  aligned-viewmap (align related-viewmap subviews x y w h)] ; align subviews
              (assoc aligned-viewmap id aligned-view)))
          viewmap
          coll))


(defn collect-visible-ids [viewmap coll path]
  "collects ids of views that are currently visible"
  (reduce
   (fn [res id]
     (let [view (viewmap id)]
       (concat res (collect-visible-ids viewmap (:subviews view) (str path ":" (:class view) )))))
   coll
   coll))


(defn collect-pressed-views [viewmap [ex ey]]
  "collects view under touch point"
  (reduce
   (fn [result view]
     (let [{:keys [id x y w h]} view]
       (if (and (and (> ex x) (< ex (+ x w))) (and (> ey y) (< ey (+ y h))))
         (conj result id)
         result)))
   []
   (vals viewmap)))


(defn touch-slider
  "touch event for slider"
  [{:keys [command subviews x y w h] :as view} viewmap {type :type  [px py] :point }]
  (let [subview (-> (get viewmap (first subviews))
                    (assoc :width {:pixel (- px x)}))]
    (if (= type "up")
      {:views [] :command {:text command :ratio (/ (- px x) w)}}
      {:views [subview] :command nil})))


(defn set-slider-value 
  [viewmap {:keys [id command subviews x y w h] :as view} ratio]
  (let [subview (-> (get viewmap (first subviews))
                    (assoc :width {:pixel (* w ratio)}))]
    (assoc viewmap (:id subview) subview)))


(defn touch-button
  "touch event for button"
  [{:keys [command subviews x y w h] :as view} viewmap {type :type [px py] :point}]
  (let [subview (if (= type "down")
                  (-> (get viewmap (first subviews))
                      (assoc :width {:pixel w}))
                  (-> (get viewmap (first subviews))
                      (assoc :width {:pixel 0})))]
    (if (= type "up")
      {:views [subview] :command {:text command :type type}}
      {:views [subview] :command {:text command :type type}})))
