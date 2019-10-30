(ns brawl.actor)

(defn init [x y]
  {:state "jump"

   :points {:head [x y]
            :neck [x y]
            :hip  [x y]
            
            :hand_a [(+ x 10.0) y]
            :hand_b [(- x 10.0) y]
            
            :elbow_a [x y]
            :elbow_b [x y]
            
            :knee_a [x y]
            :knee_b [x y]
            
            :ankle_a [x y]
            :ankle_b [x y]
            
            :base_a [x y]
            :base_b [x y]}})
