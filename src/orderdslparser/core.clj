(ns orderdslparser.core
  (:use name.choi.joshua.fnparse))

(def digit (lit-alt-seq  ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]))

(def hex (alt digit "a" "b" "c" "d" "e" "f"))

(def end (lit ";"))

(def split (lit ":"))

(def kundenummer (term #(re-find #"^[0-9]{9}$" %)))

(def customer-id (conc (lit-conc-seq ["kunde" "nummer" split]) kundenummer end))

(def payer-id (conc (lit-conc-seq ["betaler" "nummer" split]) kundenummer end))

(def order (conc customer-id (opt payer-id)))

(defn to-lower-case [token-string]
  (.toLowerCase token-string))

(def stop-words #{":" ";"})

(defn tokenizer [string]
  (map to-lower-case (re-seq #"\w+|\S|\d" string)))

(defn parse-dsl [dsl]
  (let [tokens (tokenizer dsl)]
    (prn tokens)
    (rule-match order #(println "FAILED: " %) #(println "LEFTOVER: " %2) {:remainder tokens})))