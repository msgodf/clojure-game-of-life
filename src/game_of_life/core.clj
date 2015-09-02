(ns game-of-life.core
  (:require [clojure.core.typed :refer [ann] :as typed]
            [clojure.string :as str]))

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

(typed/defalias Coordinate (typed/HVec [typed/AnyInteger typed/AnyInteger]))

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

(ann number-of-live-neighbours [Grid Coordinate -> Number])


;; we apply the rule at each cell independently, so we need an evolve cell function

(ann evolve-cell [Grid Coordinate -> State])

;; oh, and a function that takes a grid and a coordinate and gets the state of the cell at that location (this is pretty much the grid datastructure itself)

(ann cell-state [Grid Coordinate -> State])

;; how best to map a function over all cells in the grid?
;; if it is a function that takes a cell coordinate and state and returns a new state

;; which is pretty much `evolve-cell` - except for should there be a special case where we have state of cell, and state of neighbours and returns a new state?

;;(ann evolve-state [State (typed/Seq State) -> State])

;; but this is a low level function - not very top down - let's stick with top down for now

;; need a function to initialise the grid from a seed value

(ann initialise-grid [Integer typed/AnyInteger typed/AnyInteger -> Grid])

;; perhaps a function to take a seed and evolve by a certain number of steps?
(ann game [Integer Integer Boolean -> (typed/Option Grid)])


;; and a function to print out a grid
(ann display [Grid -> nil])

;; and a function to run the game from a seed, run it and print out the resulting grid
(ann run [-> nil])

;; another helper type for the print function
(ann state->string [State -> String])

;; and something I hadn't thought of - predicates for whether a cell is alive or dead
(ann cell-alive? [State -> Boolean])
(ann cell-dead? [State -> Boolean])

(defn cell-alive?
  [state]
  (if (= state :alive) true false))

(defn cell-dead?
  [state]
  (if (= state :dead) true false))

(defn cell-state
  [grid coordinate]
  (if-let [value (get grid coordinate)]
    value
    :dead))

(ann x-coordinate [Coordinate -> typed/AnyInteger])
(defn x-coordinate
  [coordinate]
  (first coordinate))

(ann y-coordinate [Coordinate -> typed/AnyInteger])
(defn y-coordinate
  [coordinate]
  (second coordinate))

(defn adjacent-coordinates
  [coordinate]
  (typed/for [x :- Integer [-1 0 1]
              y :- Integer [-1 0 1] :when (or (not= y 0) (not= x 0))] :- Coordinate
              [(+ (x-coordinate coordinate) x)
               (+ (y-coordinate coordinate) y)]))

(defn number-of-live-neighbours
  [grid coordinate]
  (count (filter cell-alive?
                 (map (partial cell-state grid)
                      (adjacent-coordinates coordinate)))))

(defn evolve-cell
  [grid coordinate]
  (if (cell-alive? (cell-state grid coordinate))
    (condp = (number-of-live-neighbours grid coordinate)
      0 :dead
      1 :dead
      2 :alive
      3 :alive
      :dead)
    (condp = (number-of-live-neighbours grid coordinate)
      3 :alive
      :dead)))

(defn state->string
  [state]
  (case state
    :alive "O"
    :dead " "))

(ann random-states [Integer Number (typed/Seq Coordinate) -> (typed/Seq (typed/HVec [Coordinate State]))])

(defn random-states
  [seed probability-alive coordinates]
  (let [rng (java.util.Random. seed)]
    (map (typed/ann-form (fn [coordinate] (if (> probability-alive (.nextDouble rng))
                                            [coordinate :alive]
                                            [coordinate :dead]))
                         [Coordinate -> (typed/HVec [Coordinate (typed/U ':alive ':dead)])])
         coordinates)))

(ann coordinate-states->grid [(typed/Seq (typed/HVec [Coordinate State])) -> (typed/Map Coordinate State)])

(defn coordinate-states->grid
  [coordinate-states]
  (reduce (typed/ann-form (fn [m kvs] (assoc m (first kvs) (second kvs)))
                          [(typed/Map Coordinate State)
                           (typed/HVec [Coordinate State]) -> (typed/Map Coordinate State)])
          {}
          coordinate-states))

(defn initialise-grid
  [seed width height]
  (let [coordinates (typed/for [x :- typed/AnyInteger (range width)
                                y :- typed/AnyInteger (range height)] :- Coordinate
                                [x y])
        random-coordinates (random-states seed 0.3 coordinates)]
    (coordinate-states->grid random-coordinates)))

;; now I have a grid structure, need to map over it

(defn tick
  [grid]
  (coordinate-states->grid
   (typed/for [cell :- (typed/HVec [Coordinate State]) grid] :- (typed/HVec [Coordinate State])
              [(first cell) (evolve-cell grid (first cell))])))

(ann first-coordinate
     [Grid -> (typed/Option Coordinate)])

(defn first-coordinate
  [grid]
  (when-let [i (first grid)]
    (key i)))

(ann grid-coordinates [Grid -> (typed/Seq Coordinate)])
(defn grid-coordinates
  [grid]
  (map (typed/ann-form (fn [me] (when me (key me)))
                       [(clojure.lang.IMapEntry Coordinate State) -> Coordinate])
       grid))

(ann x-coordinates [(typed/Seq Coordinate) -> (typed/Seq typed/AnyInteger)])

(defn x-coordinates
  [coordinates]
  (map y-coordinate coordinates))

(ann y-coordinates [(typed/Seq Coordinate) -> (typed/Seq typed/AnyInteger)])

(defn y-coordinates
  [coordinates]
  (map x-coordinate coordinates))

(ann find-width [Grid -> typed/AnyInteger])
(defn find-width
  [grid]
  (reduce (typed/ann-form (fn [m v] (if (> m v) m v))
                          [typed/AnyInteger typed/AnyInteger -> typed/AnyInteger])
          0
          (x-coordinates (grid-coordinates grid))))

(ann find-height [Grid -> typed/AnyInteger])

(defn find-height
  [grid]
  (reduce (typed/ann-form (fn [m v] (if (> m v) m v))
                          [typed/AnyInteger typed/AnyInteger -> typed/AnyInteger])
          0
          (y-coordinates (grid-coordinates grid))))

;; find the maximum x and maximum y (max (map first (keys grid)
(defn display
  [grid]
  (doall
   (typed/for [y :- typed/AnyInteger (range 0 (inc (find-height grid)))] :- nil
              (prn y (clojure.string/join ""
                                          (typed/for [x :- typed/AnyInteger (range 0 (find-width grid))] :- String
                                                     (state->string (cell-state grid [x y])))))))
  nil)

(defn game
  [seed steps display?]
  (last (take steps
              (iterate (typed/ann-form (fn [grid] (when display?
                                                    (display grid)
                                                    (println "\n\n\n")) (tick grid))
                                       [Grid -> Grid])
                       (initialise-grid seed 20 20)))))

(defn run
  []
  (game 10006 10 true)
  nil)

;; I just thought - one defining feature of the grid is its bounds!
;; plus the structure has several options - but doesn't need to be exposed (abstract)
;; one obvious representation is where the state at all coordinates in the grid is stored explicitly
;; i.e. for all coordinates in the bounds of the grid (i.e. x in [min_x,max_x] and y in [min_y,max_y] there is a stored state
;; and the other obvious representation is where we partition the coordinates into dead or alive.
;; the trade offs are around time of lookup and modification of the grid, and space taken for the state of the cells

;; so the grid has some other parameters, namely the width and height (assuming a rectangular grid).

;; but this can just be part of the internal structure of the grid


;; I've learned about core.typed/AnyInteger - which is what (range) uses, instead of Integer


;; Bugs that occurred:
;; When trying to get the type of adjacent-coordinates right, I removed the part that added the offset to the specified coordinate - so it always returned the same set of adjacent coordinates! This is a variant that should have been tested. No evolution occurred, because of this bug.
;; A bigger bug occurred in that I wasn't removing the center coordinate from the adjacent coordinates! So nine coordinates were being returned.
;; my initial (intuitive) fix for this in the for expression also didn't work!
