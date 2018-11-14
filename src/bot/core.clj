(ns bot.core
  (:require [clojure.string :as str]
            [bot.ai-minimax :refer [minimax make-move]]
            [bot.board :as b]
            [bot.check-board :as c])
  (:gen-class))

(def settings (atom {}))
(def board-history (atom (list)))

(defn save-setting [[k v]]
  (swap! settings assoc k (read-string v)))

(defn game-update [[_ k v]]
  (if (= k "field")
    (swap! board-history conj v)))

(defn make-boards [s]
  (reduce
    (fn [boards [i v]]
      (b/insert boards (mod i 7) ({"0" 1 "1" 2} v) ({0.0 6 1.0 5 2.0 4 3.0 3 4.0 2 5.0 1 6.0 0} (Math/ceil (/ (inc i) 7)))))
    [0 0 0]
    (->> (vec (str/split s #","))
         (keep-indexed (fn [i x] (if (#{"1" "0"} x) [i x]))))))
;({0.0 6 1.0 5 2.0 4 3.0 3 4.0 2 5.0 1 6.0 0} (Math/ceil (/ 1 6)))
;(b/print-board
;  (make-boards
;    "0,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,0,.,0,.,.,.,1,1,1"))

(defn get-action [time]
  (if (= 1 (count @board-history))
    3
    (make-move minimax (make-boards (first @board-history)) (inc (@settings "your_botid")) 4)))
;(get-action 1)
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