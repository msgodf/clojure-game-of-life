(ns game-of-life.core
  (:require [clojure.core.typed :refer [ann] :as typed]))

;;Rules:
;;
;;Any live cell with fewer than two live neighbours dies, as if caused by under-population.
;;Any live cell with two or three live neighbours lives on to the next generation.
;;Any live cell with more than three live neighbours dies, as if by overcrowding.
;;Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

;; what do I need? A list of cells
;; I want to map the evolution rule over each cell, and need a function that gets its eight neighbours.

;; a cell can just be a coordinate pair as key and the state as the value
;; {[x y] state}

;; then a function to find the adjacent neighbours is as follows:
;; given a coordinate, what are the adjacent coordinates?
;; [x y] -> N NE E SE S SW W NW (or perhaps [0 -1] [1 -1] [1 0] ..)

;; then a rule gets fed the cell and its neighbours, and produces a new state for the cell

(typed/defalias Coordinate (typed/HVec [Integer Integer]))

(typed/defalias State (typed/U ':alive
                               ':dead))

(typed/defalias Grid (typed/Map Coordinate State))

;; perhaps this shouldn't be an unbounded Seq, but a map from neighbour names to coordinate
(ann adjacent-coordinates [Coordinate -> (typed/Seq Coordinate)])

;; I assume that the rules are applied in order, and no more than one rule can be applied at any tick

;; the tick function takes a grid and produces a grid

(ann tick [Grid -> Grid])

;; what functions would be useful for applying the rules?

;; count of live neighbours - needs a grid and a cell coordinate

(ann number-of-live-neighbours [Grid Coordinate -> Integer])


;; we apply the rule at each cell independently, so we need an evolve cell function

(ann evolve-cell [Grid Coordinate -> State])

;; oh, and a function that takes a grid and a coordinate and gets the state of the cell at that location (this is pretty much the grid datastructure itself)

(ann cell-state [Grid Coordinate -> State])


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
