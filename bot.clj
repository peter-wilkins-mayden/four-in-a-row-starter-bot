(ns bot
  (:require [clojure.string :as str]))

(def settings (atom {}))

(def updates (atom {}))

(def board (atom []))

(defn update-board [board-string] 
  (println "updating board"))


(defn save-setting [[k v]] 
  (swap! settings assoc k (read-string v)))

(defn game-update [[about k v]] 
  (if (= k "field")
    (update-board v)
    (swap! updates assoc-in [about k] (read-string v))))

;returns a random action
(defn get-action [[action t]] 
    (str "place-disc " (rand 8)))

(defn parse-input [raw-input] 
  (let [[state & args] (str/split raw-input #"\s")]
    (case state
      "settings" (save-setting args)
      "update" (game-update args)
      "action" (println (get-action args))
      "vals" (do (println @settings) (println @updates) (println (@board)))
      "exit" (System/exit 0)
      (println "Unknown Command"))))

(defn take-input [] 
  (do 
    (parse-input (read-line))
    (recur)))

(defn -main [] (take-input))

(-main)
