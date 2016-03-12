(ns bot
  (:require [clojure.string :as str]))

(def settings {})

(defn save-setting [[k v]] 
  (assoc settings k v))

(defn save-update [[player k v]] 
  (println k))

(defn print-action [[action t]] 
  (println action))

(defn parse-input [raw-input] 
  (let [[state & args] (str/split raw-input #"\s")]
    (case state
      "settings" (save-setting args)
      "update" (save-update args)
      "action" (print-action args)
      (throw (Exception. "Invalid input given to bot."))
      )))

(defn take-input [] 
  (do 
    (parse-input (read-line))
    (recur)))

(defn -main [] (take-input))

(-main)
