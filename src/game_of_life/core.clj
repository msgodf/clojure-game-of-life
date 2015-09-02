(ns game-of-life.core
  (:require [clojure.core.typed :refer [ann] :as typed]
            [clojure.string :as str]))

(typed/defalias Coordinate (typed/HVec [typed/AnyInteger typed/AnyInteger]))

(typed/defalias State (typed/U ':alive
                               ':dead))

(typed/defalias Grid (typed/Map Coordinate State))


(ann cell-alive? [State -> Boolean])

(defn cell-alive?
  [state]
  (if (= state :alive) true false))


(ann cell-dead? [State -> Boolean])

(defn cell-dead?
  [state]
  (if (= state :dead) true false))


(ann cell-state [Grid Coordinate -> State])

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


(ann adjacent-coordinates [Coordinate -> (typed/Seq Coordinate)])

(defn adjacent-coordinates
  [coordinate]
  (typed/for [x :- Integer [-1 0 1]
              y :- Integer [-1 0 1] :when (or (not= y 0) (not= x 0))] :- Coordinate
              [(+ (x-coordinate coordinate) x)
               (+ (y-coordinate coordinate) y)]))


(ann number-of-live-neighbours [Grid Coordinate -> Number])

(defn number-of-live-neighbours
  [grid coordinate]
  (count (filter cell-alive?
                 (map (partial cell-state grid)
                      (adjacent-coordinates coordinate)))))


(ann evolve-cell [Grid Coordinate -> State])

(defn evolve-cell
  "Rules:
   * Any live cell with fewer than two live neighbours dies, as if caused by under-population.
   * Any live cell with two or three live neighbours lives on to the next generation.
   * Any live cell with more than three live neighbours dies, as if by overcrowding.
   * Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction."
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


(ann state->string [State -> String])

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


(ann initialise-grid [Integer typed/AnyInteger typed/AnyInteger -> Grid])

(defn initialise-grid
  [seed width height]
  (let [coordinates (typed/for [x :- typed/AnyInteger (range width)
                                y :- typed/AnyInteger (range height)] :- Coordinate
                                [x y])
        random-coordinates (random-states seed 0.3 coordinates)]
    (coordinate-states->grid random-coordinates)))


(ann tick [Grid -> Grid])

(defn tick
  [grid]
  (coordinate-states->grid
   (typed/for [cell :- (typed/HVec [Coordinate State]) grid] :- (typed/HVec [Coordinate State])
              [(first cell) (evolve-cell grid (first cell))])))


(ann grid-coordinates [Grid -> (typed/Seq Coordinate)])

(defn grid-coordinates
  [grid]
  (map (typed/ann-form (fn [me] (when me (key me)))
                       [(clojure.lang.IMapEntry Coordinate State) -> Coordinate])
       grid))


(ann x-coordinates [(typed/Seq Coordinate) -> (typed/Seq typed/AnyInteger)])

(defn x-coordinates
  [coordinates]
  (map x-coordinate coordinates))


(ann y-coordinates [(typed/Seq Coordinate) -> (typed/Seq typed/AnyInteger)])

(defn y-coordinates
  [coordinates]
  (map y-coordinate coordinates))


(ann find-width [Grid -> typed/AnyInteger])

(defn find-width
  [grid]
  (reduce (typed/ann-form (fn [m v] (if (> m v) m v))
                          [typed/AnyInteger typed/AnyInteger -> typed/AnyInteger])
          0
          (map inc (x-coordinates (grid-coordinates grid)))))


(ann find-height [Grid -> typed/AnyInteger])

(defn find-height
  [grid]
  (reduce (typed/ann-form (fn [m v] (if (> m v) m v))
                          [typed/AnyInteger typed/AnyInteger -> typed/AnyInteger])
          0
          (map inc (y-coordinates (grid-coordinates grid)))))


(ann display [Grid -> nil])

(defn display
  [grid]
  (doall
   (typed/for [y :- typed/AnyInteger (range 0 (inc (find-height grid)))] :- nil
              (prn (clojure.string/join ""
                                          (typed/for [x :- typed/AnyInteger (range 0 (inc (find-width grid)))] :- String
                                                     (state->string (cell-state grid [x y])))))))
  nil)

(ann game [Integer Integer Boolean -> (typed/Option Grid)])

(defn game
  [seed steps display?]
  (last (take steps
              (iterate (typed/ann-form (fn [grid] (when display?
                                                    (display grid)
                                                    (println "\n\n\n")) (tick grid))
                                       [Grid -> Grid])
                       (initialise-grid seed 20 20)))))


(ann run [-> nil])

(defn run
  []
  (game 10006 10 true)
  nil)
