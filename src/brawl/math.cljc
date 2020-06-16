(ns brawl.math)

(def MPI2 (* Math/PI 2))

(defn int-to-rgba
  "converts uint32 color css rgba"
  [color]
  (let [r (/ (float (bit-and (bit-shift-right color 24) 0xFF)) 255.0)
        g (/ (float (bit-and (bit-shift-right color 16) 0xFF)) 255.0)
        b (/ (float (bit-and (bit-shift-right color 8) 0xFF)) 255.0)
        a (/ (float (bit-and color 0xFF)) 255.0)]
    [r g b a]))

