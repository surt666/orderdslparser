(ns orderdslparser.core
  (:use name.choi.joshua.fnparse
        matchure
        matchure.compile)
  (:require [clojure.walk :as walk :only (postwalk prewalk)]
            [clojure.contrib.seq-utils :as sequ :only (positions)]))

(def hex (term #(re-find #"[0-9a-f]" %)))

(def end (lit ";"))

(def date (term #(re-find #"^\d{2}-\d{2}-\d{4}$" %)))

(def number (term #(re-find #"^[0-9]+$" %)))

(def ident (term #(re-find #"^[[a-zA-Z0-9]&&[^;]]+$" %)))

(def decimal (term #(re-find #"^[0-9]+,|\.[0-9]{2}$" %)))

(def uuid (conc (lit-conc-seq ["uuid" ":"]) (term #(re-find #"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$" %)) end))

(def saleschannel (conc (lit-conc-seq ["salgskanal" ":"]) ident end))

(def clientsystem (conc (lit-conc-seq ["klient" "system" ":"]) ident end))

(def clientuser (conc (lit-conc-seq ["klient" "bruger" ":"]) ident end))

(def clientfunction (conc (lit-conc-seq ["klient" "funktion" ":"]) ident end))

(def version (conc (lit-conc-seq ["version" ":"]) (term #(re-find #"\d\.\d+" %)) end))

(def custnumber (term #(re-find #"^[0-9]{9}$" %)))

(def prodnumber (term #(re-find #"^[0-9]{7}$" %)))

(def installationnumber (conc (lit-conc-seq ["installations" "nr" ":"]) number end))

(def address-id (conc (lit-conc-seq ["adresse" "id" ":"]) number end))

(def instaddress (conc (lit-conc-seq ["installations" "adresse" "{"]) address-id installationnumber (lit "}")))

(def customer-id (conc (lit-conc-seq ["kunde" "nr" ":"]) custnumber end))

(def payer-id (conc (lit-conc-seq ["betaler" "nr" ":"]) custnumber end))

(def dealerid (conc (lit-conc-seq ["forhandler" "id" ":"]) number end))

(def totalprice (conc (lit-conc-seq ["total" "pris" ":"]) decimal end))

(def salesagent (conc (lit-conc-seq ["salgs" "agent" ":"]) (rep+ ident) end))

(def dealer (conc (lit-conc-seq ["forhandler" "{"]) dealerid salesagent totalprice (lit "}")))

(def bool (alt (lit "true") (lit "false")))

(def legalletter (conc (lit-conc-seq ["juridisk" ":"]) bool end))

(def payerletter (conc (lit-conc-seq ["betaler" ":"]) bool end))

(def letters (conc (lit-conc-seq ["breve" "{"]) legalletter payerletter (lit "}")))

(def custandinst (conc (lit-conc-seq ["kunde" "med" "installations" "adresse" "{"]) customer-id address-id installationnumber (lit "}")))

(def custorinst (alt customer-id instaddress))

(def custinst (alt custorinst custandinst))

(def email (conc (lit-conc-seq ["email" ":"]) (term #(re-find #".+@.+\..+" %)) end))

(def mobil (conc (lit-conc-seq ["mobil" ":"]) (term #(re-find #"\d{8}|\+\d{10}" %)) end))

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

(defn handle-agreementline [line res]
  (cond-match (vec (walk/postwalk #(if (list? %) (vec %) %) line))              
              [["opret" "abonnement"] ?varenr ?sn ?gebyr ?rabat ?tlfnr ?dibsid ?levering ";"] (assoc res :abonnement {:varenr varenr :sn sn :gebyr gebyr :rabat rabat :tlfnr tlfnr :dibsid dibsid :levering levering})
              [["opret" "ydelse"] ?varenr ";"] (assoc res :ydelse {:varenr varenr})
              _ res))

(defn handle-agreementlines [lines]
   (loop [l lines res {}]
    (if (empty? l)
      res
      (let [line (first l)]
        (recur (rest l) (handle-agreementline line res))))))

(defn handle-orderline [line res]  
  (cond-match (vec (walk/postwalk #(if (list? %) (vec %) %) line))
              ["skift" ?fo "abonnement" ?fravnr ?frasn "til" ?tilvnr ?tilsn ?gebyr ?rabat [["paa" "aftale"] ?aftnr] ?levering ";"] (assoc res :skift-abonnement {:fo fo :fravnr fravnr :frasn frasn :tilvnr tilvnr :tilsn tilsn :gebyr gebyr :rabat rabat :aftnr aftnr :levering levering})
              ["opret" ?fo "abonnement" ?tilvnr ?tilsn ?gebyr ?rabat [["paa" "aftale"] ?aftnr] ?levering ";"] (assoc res :opret-abonnement {:fo fo :tilvnr tilvnr :tilsn tilsn :gebyr gebyr :rabat rabat :aftnr aftnr :levering levering})
              ["opret" ?fo "ydelse" ?tilvnr [["paa" "aftale"] ?aftnr] ";"] (assoc res :opret-ydelse {:fo fo :tilvnr tilvnr :aftnr aftnr})
              ["opsig" ?fo "abonnement" ?varenr [["pga" "aarsag"] ?aarsag] [["paa" "aftale"] ?aftnr] ?ignorer-binding ?levering ";"] (assoc res :opsig-abonnement {:fo fo :varenr varenr :aarsag aarsag :aftnr aftnr :ignorer-binding (if ignorer-binding "true" "false") :levering levering})
              [["opsig" "aftale"] ?aftnr [["pga" "aarsag"] ?aarsag] ?ignorer-binding [["gaeldende" "fra"] ?levering] ";"] (assoc res :opsig-aftale {:aftnr aftnr :aarag aarsag :ignorer-binding (if ignorer-binding "true" "false") :levering levering})
              ["opret" ?fo "aftale" "{" ?aftale-linier "}"] (assoc res :aftale (handle-agreementlines aftale-linier))
              _ res))

(defn handle-lines [lines]
  (loop [l lines res {}]
    (if (empty? l)
      res
      (let [line (first l)]
        (recur (rest l) (handle-orderline line res))))))

(defn handle-elm [elm res]
  (cond-match (vec (walk/postwalk #(if (list? %) (vec %) %) elm))
              [["uuid" ":"] ?uuid ";"] (assoc res :uuid uuid)
              [["version" ":"] ?version ";"] (assoc res :version version)
              [["salgskanal" ":"] ?salgskanal ";"] (assoc res :salgskanal salgskanal)
              [["klient" "system" ":"] ?klient-system ";"] (assoc res :klient-system klient-system)
              [["klient" "bruger" ":"] ?klient-bruger ";"] (assoc res :klient-bruger klient-bruger)
              [["klient" "funktion" ":"] ?klient-funktion ";"] (assoc res :klient-funktion klient-funktion)
              [["forhandler" "{"] [["forhandler" "id" ":"] ?id ";"] [["salgs" "agent" ":"] ?salgsagent ";"] [["total" "pris" ":"] ?pris ";"] "}"] (assoc res :forhandler {:id id :salgsagent (reduce #(str %1 " " %2) salgsagent) :pris pris})
              [["breve" "{"] [["juridisk" ":"] ?juridisk ";"] [["betaler" ":"] ?betaler ";"] "}"] (assoc res :breve {:juridisk juridisk :betaler betaler})
              [["kunde" "med" "installations" "adresse" "{"] [["kunde" "nr" ":"] ?kundenr ";"] [["adresse" "id" ":"] ?amsid ";"] [["installations" "nr" ":"] ?instnr ";"] "}"] (assoc res :kunde-med-inst-adr {:kundenr kundenr :amsid amsid :instnr instnr})
              [["ordre" "bekraeftelse" "{"] [["email" ":"] ?email ";"] [["mobil" ":"] ?mobil ";"] "}"] (assoc res :ordre-bekraeftelse {:email email :mobil mobil})
              [["ordre" "linier" "{"] ?lines "}"] (assoc res :ordrelinier (handle-lines lines))
              _ res))

(defn transform [dsl-list]
  "transform dsl list to map"
  (loop [l dsl-list res {}]
    (if (empty? l)
      res
      (let [elm (first l)]        
        (recur (rest l) (handle-elm elm res))))))

(defn parse-dsl [dsl]
  "parse dsl into list form"
  (let [tokens (tokenizer dsl)    
        res (rule-match order #(println "FAILED: " %) #(println "LEFTOVER: " %2) {:remainder tokens})]        
    (transform res)))
