(ns orderdslparser.core
  (:use name.choi.joshua.fnparse))

(def hex (term #(re-find #"[0-9a-f]" %)))

(def end (lit ";"))

(def split (lit ":"))

(def date (term #(re-find #"^\d{2}-\d{2}-\d{4}$" %)))

(def number (term #(re-find #"^[0-9]+$" %)))

(def ident (term #(re-find #"^.+$" %)))

(def decimal (term #(re-find #"^[0-9]+,[0-9]{2}$" %)))

(def uuid (conc (lit "uuid") split (term #(re-find #"^[0-9a-f]{8}$" %)) (lit "-") (term #(re-find #"^[0-9a-f]{4}$" %)) (lit "-") (term #(re-find #"^[0-9a-f]{4}$" %)) (lit "-") (term #(re-find #"^[0-9a-f]{4}$" %)) (lit "-") (term #(re-find #"^[0-9a-f]{12}$" %)) end))

(def saleschannel (conc (lit "salgskanal") split ident end))

(def clientsystem (conc (lit-conc-seq ["klient" "system"]) split ident end))

(def clientuser (conc (lit-conc-seq ["klient" "bruger"]) split ident end))

(def clientfunction (conc (lit-conc-seq ["klient" "funktion"]) split ident end))

(def version (conc (lit "version") split number (lit ".") number end))

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

(def emailaddress (conc ident (lit "@") ident (lit ".") ident))

(def email (conc (lit "email") split emailaddress end))

(def mobil (conc (lit "mobil") split (opt (lit "+")) number end))

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

(def oaa (conc (lit "opret") (opt business-area) (lit "abonnement") prodnumber (opt withsn) (opt withfee) (opt withdiscount) (opt withphonenumber) (opt anddibs) (opt withdelivery) end))

(def oa (conc (lit "opret") business-area (lit "abonnement") prodnumber (opt withsn) (opt withfee) (opt withdiscount) (opt withphonenumber) (opt anddibs) withagreement (opt withdelivery) end))

(def ska (conc (lit "skift") business-area (lit "abonnement") prodnumber (opt withsn) (lit "til") prodnumber (opt withsn) (opt withfee) (opt withdiscount) (opt withagreement) (opt withdelivery) end))

(def saa (conc (lit "opsig") business-area (lit "abonnement") prodnumber withreason (opt withagreement) (opt ignorebinding) (opt withdelivery) end))

(def sa (conc (lit-conc-seq ["opsig" "aftale"]) number withreason (opt ignorebinding) (opt withdelivery) end))
 
(def oya (conc (lit-conc-seq ["opret" "ydelse"]) prodnumber end))

(def oy (conc (lit "opret") business-area (lit "ydelse") prodnumber (opt withagreement) end))

(def oaf (conc (lit "opret") business-area (lit-conc-seq ["aftale" "{"]) (rep+ (alt oaa oya)) (lit "}")))

(def orderline (alt oaf oaa oy ska saa sa))

(def orderlines (conc (lit-conc-seq ["ordre" "linier" "{"]) (rep* orderline) (lit "}")))

(def order (conc uuid version custinst (opt payer-id) saleschannel clientsystem clientuser clientfunction (opt dealer) (opt letters) (opt orderconfirm) orderlines))

(defn to-lower-case [token-string]
  (.toLowerCase token-string))

(def stop-words #{":" ";"})            

(defn tokenizer [string]
  (re-seq #"\w+|\S|\d" string))

(defn parse-dsl [dsl]
  (let [tokens (tokenizer dsl)]
    (prn tokens)
    (rule-match order #(println "FAILED: " %) #(println "LEFTOVER: " %2) {:remainder tokens})))