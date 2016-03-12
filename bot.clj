(ns bot
  (:require [clojure.string :as str]))

(defn check-winner [board] ())

;Define the mutable program state
(def settings (atom {}))
(def updates (atom {}))
(def board (atom []))

;Update the board with parsed data (list of vectors)
;Note that it stores the given rows in reverse order from what the server
;provides... it just seemed more sensible
(defn update-board [board-string] 
  (let [vs (vec (str/split board-string #";"))] 
    (reset! board (reverse (map #(str/split % #",") vs)))))

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
    (str "place-disc " (rand 8)))

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

(-main)
