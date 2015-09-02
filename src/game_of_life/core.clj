(ns game-of-life.core
  (:require [clojure.core.typed :refer [ann] :as t]
            [clojure.string :as str]))

(t/defalias Coordinate (t/HVec [t/AnyInteger t/AnyInteger]))

(t/defalias State (t/U ':alive
                               ':dead))

(t/defalias Grid (t/Map Coordinate State))

(t/defn cell-alive?
  [state :- State] :- Boolean
  (if (= state :alive) true false))

(t/defn cell-dead?
  [state :- State] :- Boolean
  (if (= state :dead) true false))

(t/defn cell-state
  [grid :- Grid
   coordinate :- Coordinate] :- State
  (if-let [value (get grid coordinate)]
    value
    :dead))

(t/defn x-coordinate
  [coordinate :- Coordinate] :- t/AnyInteger
  (first coordinate))

(t/defn y-coordinate
  [coordinate :- Coordinate] :- t/AnyInteger
  (second coordinate))

(t/defn adjacent-coordinates
  [coordinate :- Coordinate] :- (t/Seq Coordinate)
  (t/for [x :- Integer [-1 0 1]
          y :- Integer [-1 0 1]
          :when (or (not= y 0) (not= x 0))] :- Coordinate
          [(+ (x-coordinate coordinate) x)
           (+ (y-coordinate coordinate) y)]))

(t/defn number-of-live-neighbours
  [grid :- Grid
   coordinate :- Coordinate] :- Number
  (count (filter cell-alive?
                 (map (partial cell-state grid)
                      (adjacent-coordinates coordinate)))))

(t/defn evolve-cell
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

(t/defn state->string
  [state :- State] :- String
  (case state
    :alive "O"
    :dead " "))

(t/defn random-states
  [seed :- Integer
   probability-alive :- Number
   coordinates :- (t/Seq Coordinate)] :- (t/Seq (t/HVec [Coordinate State]))
  (let [rng (java.util.Random. seed)]
    (map (t/ann-form (fn [coordinate] (if (> probability-alive (.nextDouble rng))
                                            [coordinate :alive]
                                            [coordinate :dead]))
                         [Coordinate -> (t/HVec [Coordinate (t/U ':alive ':dead)])])
         coordinates)))

(t/defn coordinate-states->grid
  [coordinate-states :- (t/Seq (t/HVec [Coordinate State]))] :- (t/Map Coordinate State)
  (reduce (t/ann-form (fn [m [k v]] (assoc m k v))
                          [(t/Map Coordinate State)
                           (t/HVec [Coordinate State]) -> (t/Map Coordinate State)])
          {}
          coordinate-states))

(t/defn initialise-grid
  [seed :- Integer
   width :- t/AnyInteger
   height :- t/AnyInteger] :- Grid
  (->> (t/for [x :- t/AnyInteger (range width)
                   y :- t/AnyInteger (range height)] :- Coordinate
                   [x y])
       (random-states seed 0.3)
       (coordinate-states->grid)))

(t/defn tick
  [grid :- Grid] :- Grid
  (coordinate-states->grid
   (t/for [cell :- (t/HVec [Coordinate State]) grid] :- (t/HVec [Coordinate State])
              [(first cell) (evolve-cell grid (first cell))])))

(t/defn grid-coordinates
  [grid :- Grid] :- (t/Seq Coordinate)
  (map (t/ann-form (fn [me] (when me (key me)))
                       [(clojure.lang.IMapEntry Coordinate State) -> Coordinate])
       grid))

(t/defn x-coordinates
  [coordinates :- (t/Seq Coordinate)] :- (t/Seq t/AnyInteger)
  (map x-coordinate coordinates))

(t/defn y-coordinates
  [coordinates :- (t/Seq Coordinate)] :- (t/Seq t/AnyInteger)
  (map y-coordinate coordinates))

(t/defn max-any-integer
  [xs :- (t/Seq t/AnyInteger)] :- t/AnyInteger
  (reduce (t/ann-form (fn [m v] (if (> m v) m v))
                          [t/AnyInteger t/AnyInteger -> t/AnyInteger])
          0
          xs))

(t/defn find-width
  [grid :- Grid] :- t/AnyInteger
  (->> grid
       (grid-coordinates)
       (x-coordinates)
       (map inc)
       (max-any-integer)))

(t/defn find-height
  [grid :- Grid] :- t/AnyInteger
  (->> grid
       (grid-coordinates)
       (y-coordinates)
       (map inc)
       (max-any-integer)))

(t/defn display
  [grid :- Grid] :- nil
  (doall
   (t/for [y :- t/AnyInteger (range 0 (inc (find-height grid)))] :- nil
              (prn (clojure.string/join ""
                                        (t/for [x :- t/AnyInteger (range 0 (inc (find-width grid)))] :- String
                                                   (state->string (cell-state grid [x y])))))))
  nil)

(t/defn game
  [seed :- Integer
   steps :- Integer
   display? :- Boolean] :- (t/Option Grid)
  (last (take steps
              (iterate (t/ann-form (fn [grid] (when display?
                                                    (display grid)
                                                    (println "\n\n\n")) (tick grid))
                                       [Grid -> Grid])
                       (initialise-grid seed 20 20)))))

(t/defn run
  [] :- nil
  (game 10006 10 true)
  nil)
