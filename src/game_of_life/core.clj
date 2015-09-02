(ns game-of-life.core
  (:require [clojure.core.typed :refer [ann] :as typed]
            [clojure.string :as str]))

(typed/defalias Coordinate (typed/HVec [typed/AnyInteger typed/AnyInteger]))

(typed/defalias State (typed/U ':alive
                               ':dead))

(typed/defalias Grid (typed/Map Coordinate State))

(typed/defn cell-alive?
  [state :- State] :- Boolean
  (if (= state :alive) true false))

(typed/defn cell-dead?
  [state :- State] :- Boolean
  (if (= state :dead) true false))

(typed/defn cell-state
  [grid :- Grid
   coordinate :- Coordinate] :- State
  (if-let [value (get grid coordinate)]
    value
    :dead))

(typed/defn x-coordinate
  [coordinate :- Coordinate] :- typed/AnyInteger
  (first coordinate))

(typed/defn y-coordinate
  [coordinate :- Coordinate] :- typed/AnyInteger
  (second coordinate))

(typed/defn adjacent-coordinates
  [coordinate :- Coordinate] :- (typed/Seq Coordinate)
  (typed/for [x :- Integer [-1 0 1]
              y :- Integer [-1 0 1]
              :when (or (not= y 0) (not= x 0))] :- Coordinate
              [(+ (x-coordinate coordinate) x)
               (+ (y-coordinate coordinate) y)]))

(typed/defn number-of-live-neighbours
  [grid :- Grid
   coordinate :- Coordinate] :- Number
  (count (filter cell-alive?
                 (map (partial cell-state grid)
                      (adjacent-coordinates coordinate)))))

(typed/defn evolve-cell
  "Rules:
   * Any live cell with fewer than two live neighbours dies, as if caused by under-population.
   * Any live cell with two or three live neighbours lives on to the next generation.
   * Any live cell with more than three live neighbours dies, as if by overcrowding.
   * Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction."
  [grid :- Grid
   coordinate :- Coordinate] :- State
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

(typed/defn state->string
  [state :- State] :- String
  (case state
    :alive "O"
    :dead " "))

(typed/defn random-states
  [seed :- Integer
   probability-alive :- Number
   coordinates :- (typed/Seq Coordinate)] :- (typed/Seq (typed/HVec [Coordinate State]))
  (let [rng (java.util.Random. seed)]
    (map (typed/ann-form (fn [coordinate] (if (> probability-alive (.nextDouble rng))
                                            [coordinate :alive]
                                            [coordinate :dead]))
                         [Coordinate -> (typed/HVec [Coordinate (typed/U ':alive ':dead)])])
         coordinates)))

(typed/defn coordinate-states->grid
  [coordinate-states :- (typed/Seq (typed/HVec [Coordinate State]))] :- (typed/Map Coordinate State)
  (reduce (typed/ann-form (fn [m [k v]] (assoc m k v))
                          [(typed/Map Coordinate State)
                           (typed/HVec [Coordinate State]) -> (typed/Map Coordinate State)])
          {}
          coordinate-states))

(typed/defn initialise-grid
  [seed :- Integer
   width :- typed/AnyInteger
   height :- typed/AnyInteger] :- Grid
  (->> (typed/for [x :- typed/AnyInteger (range width)
                   y :- typed/AnyInteger (range height)] :- Coordinate
                   [x y])
       (random-states seed 0.3)
       (coordinate-states->grid)))

(typed/defn tick
  [grid :- Grid] :- Grid
  (coordinate-states->grid
   (typed/for [cell :- (typed/HVec [Coordinate State]) grid] :- (typed/HVec [Coordinate State])
              [(first cell) (evolve-cell grid (first cell))])))

(typed/defn grid-coordinates
  [grid :- Grid] :- (typed/Seq Coordinate)
  (map (typed/ann-form (fn [me] (when me (key me)))
                       [(clojure.lang.IMapEntry Coordinate State) -> Coordinate])
       grid))

(typed/defn x-coordinates
  [coordinates :- (typed/Seq Coordinate)] :- (typed/Seq typed/AnyInteger)
  (map x-coordinate coordinates))

(typed/defn y-coordinates
  [coordinates :- (typed/Seq Coordinate)] :- (typed/Seq typed/AnyInteger)
  (map y-coordinate coordinates))

(typed/defn find-width
  [grid :- Grid] :- typed/AnyInteger
  (reduce (typed/ann-form (fn [m v] (if (> m v) m v))
                          [typed/AnyInteger typed/AnyInteger -> typed/AnyInteger])
          0
          (map inc (x-coordinates (grid-coordinates grid)))))

(typed/defn find-height
  [grid :- Grid] :- typed/AnyInteger
  (reduce (typed/ann-form (fn [m v] (if (> m v) m v))
                          [typed/AnyInteger typed/AnyInteger -> typed/AnyInteger])
          0
          (map inc (y-coordinates (grid-coordinates grid)))))

(typed/defn display
  [grid :- Grid] :- nil
  (doall
   (typed/for [y :- typed/AnyInteger (range 0 (inc (find-height grid)))] :- nil
              (prn (clojure.string/join ""
                                        (typed/for [x :- typed/AnyInteger (range 0 (inc (find-width grid)))] :- String
                                                   (state->string (cell-state grid [x y])))))))
  nil)

(typed/defn game
  [seed :- Integer
   steps :- Integer
   display? :- Boolean] :- (typed/Option Grid)
  (last (take steps
              (iterate (typed/ann-form (fn [grid] (when display?
                                                    (display grid)
                                                    (println "\n\n\n")) (tick grid))
                                       [Grid -> Grid])
                       (initialise-grid seed 20 20)))))

(typed/defn run
  [] :- nil
  (game 10006 10 true)
  nil)
