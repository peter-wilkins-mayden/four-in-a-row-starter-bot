(ns bot.core
  (:require [clojure.string :as str])
  (:gen-class))

(def board-rows 6)

(def board-cols 7)

(defn get-diagonal-4 [[col row]]
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

(def possible-4s
  (let [spaces (for 
                 [col (range board-cols) 
                  row (range board-rows) 
                  :when (or (<= col 3) (<= row 2))] 
                 [col row])] 
   (filter (fn [[& pairs]] 
             (every? #(and (< (first %) board-cols) (< (second %) board-rows)) pairs)) 
           (apply concat 
                  (map 
                    (juxt get-horizontal-4 get-vertical-4 get-diagonal-4) 
                    spaces)))))

;Define the mutable program state
(def settings (atom {}))
(def updates (atom {}))
(def board (atom []))

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

;returns a random action
(defn get-action [[action t]] 
    (str "place-disc " (rand board-cols)))

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
