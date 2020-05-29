(ns brawl.gun
  (:require [brawl.floatbuffer :as fb])
  (:use [mpd.math2 :only [rotate-up-v2 resize-v2 scale-v2 rotate-90-cw rotate-90-ccw add-v2 sub-v2]]))


(defn init
  "new gun"
  [id pos]
  {:id id
   :p pos ; point
   :d [-10 0] ; direction
   :f 1.0 ; facing
   :b 6 ; bullets
   :s 0}) ; shot frames


(defn get-skin-triangles
  "generate gun skin"
  [{:keys [d f s] [x y :as p] :p} floatbuffer [l r b t]]
  (if-not (and (< l x) (> r x) (< t y) (> b y)) 
    floatbuffer
    (let [hor40 (resize-v2 d 40.0)
          hor10 (resize-v2 d 10.0)
          hor15 (resize-v2 d 15.0)
          hor5 (resize-v2 d -2.0)
          vert40 (if (> f 0) (rotate-90-cw hor40) (rotate-90-ccw hor40))
          vert20 (resize-v2 vert40 20)
          vert5 (resize-v2 vert20 5)
          hor100 (resize-v2 d 500.0)
          [a b :as trans] (add-v2 p (resize-v2 vert5 -10.0))
          [c d :as phor40] (add-v2 trans hor40)
          [e f :as phor40vert5] (add-v2 trans (add-v2 hor40 vert5))
          [g h :as pvert5] (add-v2 trans vert5)
          [i j :as phor15] (add-v2 trans hor15)
          [k l :as phor10vert20] (add-v2 trans (add-v2 hor10 vert20))
          [m n :as phor5vert20] (add-v2 trans (add-v2 hor5 vert20))
          [o p :as phor100] (add-v2 trans hor100)]
      
      (cond-> floatbuffer
        true (fb/append! (array a b 0 0 0 1
                                c d 0 0 0 1
                                e f 0 0 0 1
                                a b 0 0 0 1
                                e f 0 0 0 1
                                g h 0 0 0 1
                                
                                a b 0 0 0 1
                                i j 0 0 0 1
                                k l 0 0 0 1
                                a b 0 0 0 1
                                k l 0 0 0 1
                                m n 0 0 0 1))
        (and (< s 5) (> s 0)) (fb/append! (array c d 0.9 0.9 0 1.0
                                                 o p 0.9 0.9 0 1.0
                                                 e f 0.9 0.9 0 1.0))))))
                                                  
