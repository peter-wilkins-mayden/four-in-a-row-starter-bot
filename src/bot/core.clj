(ns bot.core
  (:require [clojure.string :as str])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def board-rows 6)

(def board-cols 7)

(defn get-left-diagonal-4 [[col row]]
  (for [x (range (- col 3) (+ 1 col))
        y (range row (+ row 4))
        :when (= (+ x y) (+ col row))]
    [x y]))

(defn get-right-diagonal-4 [[col row]]
  (for [x (range col (+ col 4))
        y (range row (+ row 4))
        :when (= (- x y) (- col row))]
    [x y]))


(defn get-vertical-4 [[col row]]
  (for [ y (range row (+ row 4))
        :let [x col]]
    [x y]))

(defn get-horizontal-4 [[col row]]
  (for [x (range col (+ col 4))
        :let [y row]]
    [x y]))

;Returns a list of all possible rows of 4 that can win the match
(def possible-4s
  (let [spaces (for 
                 [col (range board-cols) 
                  row (range board-rows) 
                  :when (or (<= col 3) (<= row 2))] 
                 [col row])] 
   (filter (fn [[& pairs]] 
             (every? #(and (< (first %) board-cols) 
                           (< (second %) board-rows) 
                           (>= (first %) 0)) 
                     pairs)) 
           (apply concat 
                  (map 
                    (juxt get-horizontal-4 
                          get-vertical-4 
                          get-right-diagonal-4 
                          get-left-diagonal-4) 
                    spaces)))))

;Define the mutable setting maps
(def settings (atom {}))
(def updates (atom {}))
(def board (atom []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update Settings ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Update the board with parsed data (list of vectors)
;Rows are stored from bottom up, contrary to servers top down
(defn update-board [s] 
  (reset! board 
          (reverse (map #(map read-string (str/split % #","))
                              (str/split s #";")))))

;Save settings
(defn save-setting [[k v]] 
  (swap! settings assoc k (read-string v)))

;Save game updates
(defn game-update [[about k v]] 
  (if (= k "field")
    (update-board v)
    (swap! updates assoc-in [about k] (read-string v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Algorithm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Returns value in 'b' at (x, y)/(col, row)
(defn get-cell [[col row]]
  (nth (nth @board row) col))

;returns the person who connected 4, or false 
;if neither did
(defn connect-4? [cells] 
  (let [four (map get-cell cells)] 
    (cond 
      (every? #{1} four) 1
      (every? #{2} four) 2
      :else false
      )))

(defn check-winner [] 
  (some identity (map connect-4? possible-4s)))

(defn get-possible-moves [] 
  (loop [top-row (last @board) moves '() i 0] 
    (if (empty? top-row)
      moves
    (recur (rest top-row) 
           (if (= (first top-row) 0) (cons i moves) moves) 
           (inc i)))))

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
      "vals" (do (println @settings) (println @updates) (println @board))
      "exit" (System/exit 0)
      (println "Unknown Command"))))

;Initiate an input/output loop
(defn take-input [] 
  (do 
    (parse-input (read-line))
    (recur)))

(defn -main [] (take-input))
