(ns gui.ui
  (:require [gui.texmap :as texmap]
            [gui.bitmap :as bitmap]
            [gui.webgl :as webgl]
            [clojure.string :as str]))

;; generator

(defn get-value [text]
  "extract measurement, value and element from alignment/dimension value"
  (let [[element value measure] (str/split text #" ")]
    (if (= measure nil)
      (if (= value "px") ; two element value for width and height , "100 px" or "50 %"
        {:pixel (js/parseInt element)}
        {:ratio (/ (js/parseInt element) 100)})
      (if (= measure "px") ; three element value for alignments , "Edge 50 %" or "Button 5 px"
        {:element (keyword element) :pixel (js/parseInt value)}
        {:element (keyword element) :ratio (/ (js/parseInt value) 100.0)}))))


(defn gen-from-desc [viewmap desclist]
  "generate view structure from description"
  (reduce
   (fn [oldmap viewdesc]
     (let [subviews   (:subviews viewdesc)
           subids     (if subviews (map (fn [desc] (keyword (:id desc))) subviews) []) ; finalsubviews property needs ids only
           subviewmap (if subviews (gen-from-desc oldmap subviews) oldmap) ; generate subviews unto viewmap
           view (reduce
                 (fn [result pair]
                   (let [k (key pair)
                         v (val pair)]
                   (case k
                     :id       (assoc result :id (keyword v))
                     :class    (assoc result :class v)
                     :command  (assoc result :command v)
                     :color    (assoc result :color v)
                     :width    (assoc result :width (get-value v))
                     :height   (assoc result :height (get-value v))
                     :center-x (assoc result :center-x (get-value v))      
                     :center-y (assoc result :center-x (get-value v))
                     :left     (assoc result :left (get-value v))
                     :right    (assoc result :right (get-value v))
                     :top      (assoc result :top (get-value v))
                     :bottom   (assoc result :bottom (get-value v))
                     :subviews (assoc result :subviews subids)
                     (assoc result (key pair) v)))) {:x 0.0 :y 0.0 :w 150.0 :h 50.0 :subviews []} viewdesc)]
       ;; generate label if necessary
       (assoc subviewmap (:id view) view)))
   viewmap
   desclist))

(defn get-base-ids [desclist]
  (map (fn [desc] (keyword (:id desc))) desclist))
  
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

  
(defn gen-id [length]
  "generates fixed length alfanumeric hash"
   (let [chars (map char (concat (range 48 57) (range 65 90) (range 97 122)))
         id (take length (repeatedly #(rand-nth chars)))]
     (reduce str id)))


(defn add-subview [{subviews :subviews :as view} subview]
  "inserts subview's id to views subviews property"
  (assoc view :subviews (conj subviews (subview :id))))


(defn add-align [view top bottom left right center-x center-y]
  "add alignment properties to view"
  (-> view
      (assoc :top top)
      (assoc :bottom bottom)
      (assoc :left left)
      (assoc :right right)
      (assoc :center-x center-x)
      (assoc :center-y center-y)))

;; alignment

(defn align-view [viewmap id cx cy width height]
  "aligns view"
  (let [view (get viewmap id)
        {:keys [x y top bottom left right ver hor class text] w :width h :height } view
        result
        (-> view
            (assoc :x (cond
                        ;; align to view on the left or to screen edge
                        (not= left nil)
                        (if (= left "0")
                          0
                          (let [leftview ((keyword left) viewmap)]
                                (+ cx (:x leftview) (:width leftview))))
                        ;; align to view on the right or to screen edge
                        (not= right nil)
                        (if (= right "0")
                          (- width w)
                          (let [rightview ((keyword right) viewmap)]
                            (- (:x rightview) w)))
                        ;; align to horizontal center or between left align and right align view
                        (not= hor nil)
                        (if (= hor "0")
                          (+ cx (- (/ width 2) (/ w 2)))
                          (let [leftview ((keyword left) viewmap)
                                rightview ((keyword right) viewmap)]
                            (-
                             (-
                              (:x leftview)
                              (/ (- (:x rightview) (+ (:x leftview) (:width leftview)) ) 2 ) )
                             (/ w 2))))
                        ;; or leave x position as is
                        :default
                        x))
            (assoc :y (cond
                        ;; align to view on the top or to screen edge
                        (not= top nil)
                        (if (= top "0")
                          0
                          (let [topview ((keyword top) viewmap)]
                          (+ (:y topview)(:height topview))))
                        ;; align to view on the bottom or to screen edge
                        (not= bottom nil)
                        (if (= bottom "0")
                          (- height h)
                          (let [bottomview ((keyword bottom) viewmap)]
                          (- (:y bottomview) h)))
                        ;; align to vertical center or between bottom and top align view
                        (not= ver nil)
                        (if (= ver "0")
                          (+ cy (- (/ height 2) (/ h 2)))
                          (let [topview ((keyword top) viewmap)
                                bottomview ((keyword bottom) viewmap)]
                          (- (- (:y bottomview) (/ (- (:y bottomview)(+ (:y topview)(:height topview))) 2 )) (/ h 2))))
                        :default
                        y)))]
    ;(println "a:" id class text x y width height
    ;         "to" cx cy width height
    ;         "final" (result :x) (result :y) (result :width) (result :height))
    result))


(defn align [viewmap coll cx cy width height]
  "iterate through all views and align them based on their alignment switches"
  (reduce (fn [oldmap id]
            (let [view (get oldmap id)
                  {:keys [x y top bottom left right ver hor] w :width h :height} view
                  ;; filter nil and 0 switches, 0 means to full parent view
                  toalign (filter #(and (not= % nil) (not= % "0")) [top bottom left right ver hor])
                  ;; first align relative views
                  newmap (align oldmap toalign (+ cx x) (+ cy y) width height)
                  ;; align self
                  newview (align-view newmap id cx cy width height)
                  ;; align subviews
                  newnewmap (align newmap (:subviews newview) (:x newview) (:y newview) (:width newview) (:height newview))]
              (assoc newnewmap id newview)))
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


(defn collect-pressed-views [viewmap event]
  "collects view under touch point"
  (reduce
   (fn [result view]
     (let [{:keys [id x y] w :width h :height} view
           px (:x event)
           py (:y event)]
       (if (and (and (> px x) (< px (+ x w))) (and (> py y) (< py (+ y h))))
         (conj result id)
         result)))
   []
   (vals viewmap)))
