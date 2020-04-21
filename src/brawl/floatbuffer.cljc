(ns brawl.floatbuffer)


(defn create! [ ]
  {:data (js/Float32Array. 512)
   :index 0
   :length 512 })

(defn append! [ {:keys [data index length] :as buffer} numbers ]
  (let [news (count numbers)
        newb (if (< (+ index news) length)
               buffer
               (let [newarray (js/Float32Array. (* length 2))]
                 (.set newarray data)
                 {:data newarray
                  :index index
                  :length (* length 2)}))]
    (.set (:data newb) numbers index)
    (update newb :index + news)))

(defn empty! [ buffer ]
  (assoc buffer :index 0))

;(let [test (create!)
;      test1 (append! test [5.6 7.7 8.8 9.9 10.1])
;      test2 (append! test1 [5.6 7.7 8.8 9.9 10.1])]
;  test2)
