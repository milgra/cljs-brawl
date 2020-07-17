; texture map
; places smaller bitmaps into one big bitmap to use it as gpu texture

(ns gui.texmap
  (:require
    [gui.bitmap :as bitmap]))


(defn init
  "create texture map with given background color"
  [w h r g b a]
  (let [result {:texbmp (bitmap/init w h 0 0 0 0)
                :contents {}
                :changed true
                :lasth 0
                :lastx 0
                :lasty 0}]
    result))


(defn reset
  [texmap]
  (let [{texbmp :texbmp} texmap]
    (assoc texmap
      :texbmp (bitmap/clear texbmp 0 0 0 0)
      :contents {}
      :changed true
      :lasth 0
      :lastx 0
      :lasty 0)))


(defn hasbmp?
  "texmap contains bitmap with given id?"
  [texmap id]
  (let [{contents :contents} texmap]
    (contains? contents id)))


(defn getbmp
  "returns bitmap with given id"
  [texmap id]
  (let [{contents :contents} texmap]
    (get contents id)))


(defn setbmp
  "adds bmp to texmap with given id"
  [texmap bitmap texid inset]
  (let [{:keys [texbmp contents lastx lasty lasth]} texmap
        {:keys [data width height]} bitmap
        ;;new height is 0 if entering new row else check if we have to increase it
        newh (if (> (+ lastx width) (texbmp :width))
               height
               (if (> height lasth)
                 height
                 lasth))

        ;;increase new y position if entering new row
        newy (if (> (+ lastx width) (texbmp :width))
               (+ lasty lasth)
               lasty)

        ;;jump to row start if entering new row
        newx (if (> (+ lastx width) (texbmp :width))
               0
               lastx)

        texw  (+ newx width)
        texh  (+ newy height)

        newtlx (/ (+ newx inset) (texbmp :width))
        newtly (/ (+ newy inset) (texbmp :height))
        newbrx (/ (- texw inset) (texbmp :width))
        newbry (/ (- texh inset) (texbmp :height))

        over? (> newh (texbmp :height))]

    (if over?
      texmap
      (-> texmap
        (assoc-in [:contents texid] [newtlx newtly newbrx newbry])
        (assoc :lastx texw)
        (assoc :lasty newy)
        (assoc :lasth newh)
        (assoc :texbmp (bitmap/insert texbmp bitmap newx newy))
        (assoc :changed true)))))
