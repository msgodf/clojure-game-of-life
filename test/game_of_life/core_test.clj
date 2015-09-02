(ns game-of-life.core-test
  (:require [clojure.test.check :as tc])
  (:require [clojure.test.check.generators :as gen])
  (:require [clojure.test.check.properties :as prop])
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [game-of-life.core :refer :all]))

(def generate-coordinate (gen/vector gen/int 2))

(defspec always-eight-neighbours
  100
  (prop/for-all [coordinate generate-coordinate]
                (= 8 (count (adjacent-coordinates coordinate)))))

(defspec initialised-grid-height-is-same-as-found
  100
  (prop/for-all [height gen/pos-int]
                (= height (find-height (initialise-grid 100 10 height)))))

(defspec initialised-grid-width-is-same-as-found
  100
  (prop/for-all [width gen/pos-int]
                (= width (find-width (initialise-grid 100 width 10)))))


(defspec different-coordinates-give-different-neighbours
  100
  (prop/for-all [coordinates (gen/bind generate-coordinate
                                       (fn [other] (gen/tuple (gen/return other)
                                                              (gen/such-that #(not= other %)
                                                                             generate-coordinate))))]
                (not= (adjacent-coordinates (first coordinates))
                      (adjacent-coordinates (second coordinates)))))

(defspec lone-cell-anywhere-dies-within-one-tick
  100
  (prop/for-all [coordinate generate-coordinate]
                (= :dead
                   (cell-state (tick {coordinate :alive})
                               coordinate))))
