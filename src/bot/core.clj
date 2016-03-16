(ns bot.core
  (:require [clojure.string :as str])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Define the mutable setting maps
(def settings (atom {}))
(def updates (atom {}))
(def current-board (atom []))

(defn board-rows [] (get @settings "field_rows"))

(defn board-cols [] (get @settings "field_columns"))

(defn get-left-diagonal-x [l [col row]]
  (for [x (range (- col (- l 1)) (+ 1 col))
        y (range row (+ row l))
        :when (= (+ x y) (+ col row))]
    [x y]))

(defn get-right-diagonal-x [l [col row]]
  (for [x (range col (+ col l))
        y (range row (+ row l))
        :when (= (- x y) (- col row))]
    [x y]))


(defn get-vertical-x [l [col row]]
  (for [ y (range row (+ row l))
        :let [x col]]
    [x y]))

(defn get-horizontal-x [l [col row]]
  (for [x (range col (+ col l))
        :let [y row]]
    [x y]))

;Returns a list of all possible rows of x
(defn possible-streaks-nomem [x]
  (let [spaces (for 
                 [col (range (board-cols)) 
                  row (range (board-rows)) 
                  :when (or (<= col (- (board-cols) x)) (<= row (- (board-rows) x)))] 
                 [col row])] 
   (filter (fn [[& pairs]] 
             (every? #(and (< (first %) (board-cols)) 
                           (< (second %) (board-rows)) 
                           (>= (first %) 0)) 
                     pairs)) 
           (apply concat 
                  (map 
                    (juxt (partial get-horizontal-x x)
                          (partial get-vertical-x x)
                          (partial get-right-diagonal-x x)
                          (partial get-left-diagonal-x x))
                    spaces)))))

(def possible-streaks (memoize possible-streaks-nomem))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update Settings ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Update the board with parsed data (list of vectors)
;Rows are stored from bottom up, contrary to servers top down
(defn update-current-board [s] 
  (reset! current-board 
          (vec (reverse 
            (map (comp vec #(map read-string (str/split % #","))) (str/split s #";"))))))

;Save settings
(defn save-setting [[k v]] 
  (swap! settings assoc k (read-string v)))

;Save game updates
(defn game-update [[about k v]] 
  (if (= k "field")
    (update-current-board v)
    (swap! updates assoc-in [about k] (read-string v))))

;returns player id (1 or 2)
(defn player-id [] 
  (get @settings "your_botid"))

(defn opposite-player [player] 
  (if (= player 1) 2 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Algorithm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def max-depth 5)

(def infinity 999999999)

;Returns value in 'b' at (x, y)/(col, row)
(defn get-cell [board [col row]]
  (get-in board [row col]))

;Check if all the cells in row are owned by the same player
(defn connected? [board cells] 
  (let [row (map (partial get-cell board) cells)] 
    (cond 
      (every? #{1} row) 1
      (every? #{2} row) 2
      :else false
      )))

;Returns a map where the key is player,
; and value is the number of streaks
; of length 'streak-len' on 'board'
(def check-streaks (memoize (fn [board streak-len] 
  (dissoc (frequencies (map (partial connected? board) 
             (possible-streaks streak-len))) 
          false))))

(def get-board-score (memoize (fn [board depth] 
  (let [winner (set (keys (check-streaks board 4))) 
        id (player-id) 
        opponent (opposite-player (player-id))] 
    (cond
      (empty? winner) (- (- (+ (* (get (check-streaks board 3) id 0) 100) 
                               (* (get (check-streaks board 2) id 0) 10))
                            (+ (* (get (check-streaks board 3) opponent 0) 1000)
                               (* (get (check-streaks board 2) opponent 0) 10)))
                         depth)
      (contains? winner id) (- infinity depth)
      :else (- depth infinity)
      )))))

(def get-possible-moves (memoize (fn [board] 
  (loop [top-row (last board) moves '() i 0] 
    (if (empty? top-row)
      moves
    (recur (rest top-row) 
           (if (= (first top-row) 0) (cons i moves) moves) 
           (inc i)))))))


;Simulate a user placing a chip in board, returns new board

(def simulate-move (memoize (fn [board col player] 
    (let [row (.indexOf (map #(nth % col) board) 0)] 
      (assoc-in board [row col] player)))))

(declare minimax)

(defn minimax-nomemo [depth bs player move]
  (let [max-or-min (if (= player (player-id)) min max)
        boards (map #(simulate-move % move player) bs)]
  (if (= depth max-depth) 
    (apply max-or-min (map #(get-board-score % depth) boards))
    (apply max-or-min (map  #(minimax (inc depth) boards (opposite-player player) %)
                     (set (flatten (map get-possible-moves boards))))))))

(def minimax (memoize minimax-nomemo))

(defn val-for-highest-key [s]
  (second (apply max-key first s)))

(defn get-action [[action t]] 
  (let [depth 1 
        player (player-id) 
        board @current-board] 
    (val-for-highest-key 
      (for [m (get-possible-moves board)
            :let [score (minimax depth [board] (player-id) m)]]
        [score m]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logic Loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Parse input from game host
(defn parse-input [raw-input] 
  (let [[state & args] (str/split raw-input #"\s")]
    (case state
      "settings" (save-setting args)
      "update" (game-update args)
      "action" (println (str "place_disc " (get-action args)))
      "exit" (System/exit 0)
      (println "Unknown Command"))))

;Initiate an input/output loop
(defn take-input [] 
  (do 
    (parse-input (read-line))
    (recur)))

(defn -main [] (take-input))

