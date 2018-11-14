(ns bot.core
  (:require [clojure.string :as str])
  (:gen-class))

(def settings (atom {}))
(def board-history (atom (list)))

(defn save-setting [[k v]]
  (swap! settings assoc k (read-string v)))

(defn game-update [[_ k v]]
  (if (= k "field")
    (swap! board-history conj v)))

(defn get-action [time]
  (rand-int 7))

(defn parse-input [raw-input]
  (let [[state & args] (str/split raw-input #"\s")]
    (case state
      "settings" (save-setting args)
      "update" (game-update args)
      "action" (println (str "place_disc " (get-action args)))
      (println "Unknown Command"))))

(defn take-input []
  (do
    (parse-input (read-line))
    (recur)))

(defn -main [] (take-input))