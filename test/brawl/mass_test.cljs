(ns brawl.mass-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [brawl.mass :as mass]))

(deftest init-success
  (let [result
         {:trans [150.0 150.0]
          :basis [0.0 0.0]
          :weight 0.0
          :radius 0.0
          :elasticity 0.0
          :segmentgroups []}]
  (is (= result (mass/mass2 150.0 150.0 0.0 0.0 0.0)))))

(deftest init-failure
  (let [result
         {:trans [150.0 150.0]
          :basis [0.0 0.0]
          :weight 0.0
          :radius 0.0
          :elasticity 0.0
          :segmentgroups []}]
  (is (not= result (mass/mass2 250.0 250.0 0.0 0.0 0.0)))))
