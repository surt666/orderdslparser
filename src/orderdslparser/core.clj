(ns orderdslparser.core
  (:use name.choi.joshua.fnparse)
  (:require [clojure.walk :as walk :only (postwalk prewalk)]
            [clojure.contrib.seq-utils :as sequ :only (positions)]))

(def hex (term #(re-find #"[0-9a-f]" %)))

(def end (lit ";"))

(def split (lit ":"))

(def date (term #(re-find #"^\d{2}-\d{2}-\d{4}$" %)))

(def number (term #(re-find #"^[0-9]+$" %)))

(def ident (term #(re-find #"^.+$" %)))

(def decimal (term #(re-find #"^[0-9]+,[0-9]{2}$" %)))

(def uuid (conc (lit "uuid") split (term #(re-find #"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$" %)) end))

(def saleschannel (conc (lit "salgskanal") split ident end))

(def clientsystem (conc (lit-conc-seq ["klient" "system"]) split ident end))

(def clientuser (conc (lit-conc-seq ["klient" "bruger"]) split ident end))

(def clientfunction (conc (lit-conc-seq ["klient" "funktion"]) split ident end))

(def version (conc (lit "version") split (term #(re-find #"\d\.\d+" %)) end))

(def custnumber (term #(re-find #"^[0-9]{9}$" %)))

(def prodnumber (term #(re-find #"^[0-9]{7}$" %)))

(def installationnumber (conc (lit-conc-seq ["installations" "nr"]) split number end))

(def address-id (conc (lit-conc-seq ["adresse" "id"]) split number end))

(def instaddress (conc (lit-conc-seq ["installations" "adresse" "{"]) address-id installationnumber (lit "}")))

(def customer-id (conc (lit-conc-seq ["kunde" "nr"]) split custnumber end))

(def payer-id (conc (lit-conc-seq ["betaler" "nr"]) split custnumber end))

(def dealerid (conc (lit-conc-seq ["forhandler" "id"]) split number end))

(def totalprice (conc (lit-conc-seq ["total" "pris"]) split decimal end))

(def salesagent (conc (lit-conc-seq ["salgs" "agent"]) split ident (opt (rep* ident)) end))

(def dealer (conc (lit-conc-seq ["forhandler" "{"]) dealerid salesagent totalprice (lit "}")))

(def bool (alt (lit "true") (lit "false")))

(def legalletter (conc (lit "juridisk") split bool end))

(def payerletter (conc (lit "betaler") split bool end))

(def letters (conc (lit-conc-seq ["breve" "{"]) legalletter payerletter (lit "}")))

(def custandinst (conc (lit-conc-seq ["kunde" "med" "installations" "adresse" "{"]) customer-id address-id installationnumber (lit "}")))

(def custorinst (alt customer-id instaddress))

(def custinst (alt custorinst custandinst))

(def email (conc (lit "email") split (term #(re-find #".+@.+\..+" %)) end))

(def mobil (conc (lit "mobil") split (term #(re-find #"\d{8}|\+\d{10}" %)) end))

(def orderconfirm (conc (lit-conc-seq ["ordre" "bekraeftelse" "{"]) email mobil (lit "}")))

(def business-area (alt (lit "CLEAR") (lit "BB") (lit "DTV") (lit "TLF") (lit "MoBB")))

(def withfee (conc (lit-conc-seq ["med" "gebyr"]) prodnumber))

(def withdiscount (conc (lit-conc-seq ["med" "rabat"]) number))

(def withagreement (conc (lit-conc-seq ["paa" "aftale"]) number))

(def withphonenumber (conc (lit-conc-seq ["med" "tlf" "nr"]) number))

(def anddibs (conc (lit-conc-seq ["og" "dibs" "id"]) number))

(def withsn (conc (lit-conc-seq ["med" "sn"]) number))

(def withdelivery (conc (lit-conc-seq ["gaeldende" "fra"]) date))

(def withreason (conc (lit-conc-seq ["pga" "aarsag"]) number))

(def ignorebinding (lit-conc-seq ["og" "ignorer" "binding"]))

(def oaa (conc (lit-conc-seq ["opret" "abonnement"]) prodnumber (opt withsn) (opt withfee) (opt withdiscount) (opt withphonenumber) (opt anddibs) (opt withdelivery) end))

(def oa (conc (lit "opret") business-area (lit "abonnement") prodnumber (opt withsn) (opt withfee) (opt withdiscount) (opt withphonenumber) (opt anddibs) withagreement (opt withdelivery) end))

(def ska (conc (lit "skift") business-area (lit "abonnement") prodnumber (opt withsn) (lit "til") prodnumber (opt withsn) (opt withfee) (opt withdiscount) (opt withagreement) (opt withdelivery) end))

(def saa (conc (lit "opsig") business-area (lit "abonnement") prodnumber withreason (opt withagreement) (opt ignorebinding) (opt withdelivery) end))

(def sa (conc (lit-conc-seq ["opsig" "aftale"]) number withreason (opt ignorebinding) (opt withdelivery) end))
 
(def oya (conc (lit-conc-seq ["opret" "ydelse"]) prodnumber end))

(def oy (conc (lit "opret") business-area (lit "ydelse") prodnumber (opt withagreement) end))

(def oaf (conc (lit "opret") business-area (lit "aftale") (lit "{") (rep+ (alt oaa oya)) (lit "}")))

(def orderline (alt oaf oaa oy ska saa sa))

(def orderlines (conc (lit-conc-seq ["ordre" "linier" "{"]) (rep+ orderline) (lit "}")))

(def order (conc uuid version custinst (opt payer-id) saleschannel clientsystem clientuser clientfunction (opt dealer) (opt letters) (opt orderconfirm) orderlines))

(defn to-lower-case [token-string]
  (.toLowerCase token-string))     

(defn tokenizer [string]
  (re-seq #"[a-zA-Z_0-9\-.@+]+|\S+" string))

(defn pos-colon [elm]    
  (first (sequ/positions #(= ":" %) elm)))

(defn pos-curly [elm]    
  (first (sequ/positions #(= "{" %) elm)))

(defn make-key [e]  
  (cond
   (pos-colon e) (keyword (reduce str (flatten (take (pos-colon e) e))))
   (pos-curly e) (keyword (reduce str (flatten (take (pos-curly e) e))))))

(defn make-val [e]
  (cond
   (pos-colon e) (reduce str (take-last (- (count e) (pos-colon e) 2) (filter #(not (= ";" %)) e)))
   (pos-curly e) (reduce str (take-last (- (count e) (pos-curly e) 2) (filter #(not (= ";" %)) e)))))

(defn line? [e]
  (= ";" (last e)))

(defn block? [e]
  (not (line? e)))

(defn parse-vec [v]
  (prn "V" v))

(defn parse-block [elm]
  (loop [e elm res {}]
    (if (empty? e)
      res
      (recur (rest e) (if (line? (first e))
                        (assoc res (make-key (first e)) (make-val (first e)))
                        (when (not (= "}" (first e)))
                          (if (vector? (first e))
                            (parse-vec (first e))
                            (prn "B" (first e)))))))))

(defn parse-elm [elm]  
  (cond
   (line? elm) (assoc {} (make-key elm) (make-val elm))
   (block? elm) (parse-block elm)))

(defn parse-dsl [dsl]
  (let [tokens (tokenizer dsl)    
        res (rule-match order #(println "FAILED: " %) #(println "LEFTOVER: " %2) {:remainder tokens})]    
    ;; (map #(parse-elm %) res)
    res))
