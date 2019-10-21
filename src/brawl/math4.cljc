(ns brawl.math4)

(defn proj_ortho [ left right bottom top near far ]
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

(defn proj_pers [ fovy aspect nearz farz ]
  (let [cotan (/ 1.0 (Math/tan (/ fovy 2.0)))]
    [( / cotan aspect )
     0.0
     0.0
     0.0
     
     0.0
     cotan
     0.0
     0.0

     0.0
     0.0
     (/ (+ farz nearz) (- farz nearz))
     -1.0

     0.0
     0.0
     (/ (* 2.0 farz nearz) (0 nearz farz))
     0.0] ))
