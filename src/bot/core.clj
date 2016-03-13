(ns bot.core
  (:require [clojure.string :as str])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def board-rows 6)

(def board-cols 7)

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
(defn possible-xs [x]
  (let [spaces (for 
                 [col (range board-cols) 
                  row (range board-rows) 
                  :when (or (<= col (- board-cols x)) (<= row (- board-rows x)))] 
                 [col row])] 
   (filter (fn [[& pairs]] 
             (every? #(and (< (first %) board-cols) 
                           (< (second %) board-rows) 
                           (>= (first %) 0)) 
                     pairs)) 
           (apply concat 
                  (map 
                    (juxt (partial get-horizontal-x x)
                          (partial get-vertical-x x)
                          (partial get-right-diagonal-x x)
                          (partial get-left-diagonal-x x))
                    spaces)))))

(def possible-4s (possible-xs 4))
(def possible-3s (possible-xs 3))
(def possible-2s (possible-xs 2))

;Define the mutable setting maps
(def settings (atom {}))
(def updates (atom {}))
(def current-board (atom []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update Settings ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Update the board with parsed data (list of vectors)
;Rows are stored from bottom up, contrary to servers top down
(defn update-current-board [s] 
  (reset! current-board 
          (reverse (map #(map read-string (str/split % #","))
                              (str/split s #";")))))

;Save settings
(defn save-setting [[k v]] 
  (swap! settings assoc k (read-string v)))

;Save game updates
(defn game-update [[about k v]] 
  (if (= k "field")
    (update-current-board v)
    (swap! updates assoc-in [about k] (read-string v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Algorithm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def max-depth 4)

;Returns value in 'b' at (x, y)/(col, row)
(defn get-cell [board [col row]]
  (nth (nth board row) col))

;returns the person who connected 4, or false 
;if neither did
(defn connect-4? [board cells] 
  (let [four (map (partial get-cell board) cells)] 
    (cond 
      (every? #{1} four) 1
      (every? #{2} four) 2
      :else false
      )))

(defn check-winner [board] 
  (some identity 
        (map (partial connect-4? board) possible-4s)))

(defn get-possible-moves [board] 
  (loop [top-row (last board) moves '() i 0] 
    (if (empty? top-row)
      moves
    (recur (rest top-row) 
           (if (= (first top-row) 0) (cons i moves) moves) 
           (inc i)))))

(defn get-board-score [board depth opponent?] ())

;returns a random action
(defn get-action [[action t]] 
    (str "place-disc " (rand board-cols)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logic Loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Parse input from game host
(defn parse-input [raw-input] 
  (let [[state & args] (str/split raw-input #"\s")]
    (case state
      "settings" (save-setting args)
      "update" (game-update args)
      "action" (println (get-action args))
      "vals" (do (println @settings) (println @updates) (println @current-board))
      "exit" (System/exit 0)
      (println "Unknown Command"))))

;Initiate an input/output loop
(defn take-input [] 
  (do 
    (parse-input (read-line))
    (recur)))

(defn -main [] (take-input))
