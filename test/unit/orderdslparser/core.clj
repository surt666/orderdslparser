(ns unit.orderdslparser.core
  (:use orderdslparser.core
        lazytest.context.stub
        [lazytest.describe :only (describe it given do-it for-any with before)]))

(describe parse-dsl
  (it "parses dsl1"    
    (not (nil? (:uuid (parse-dsl (slurp "test/unit/orderdslparser/dsl1.txt"))))))
  (it "parses dsl2"    
    (not (nil? (:uuid (parse-dsl (slurp "test/unit/orderdslparser/dsl2.txt"))))))
  (it "parses dsl3"
    (not (nil? (:uuid (parse-dsl (slurp "test/unit/orderdslparser/dsl3.txt"))))))
  (it "parses dsl4"
    (not (nil? (:uuid (parse-dsl (slurp "test/unit/orderdslparser/dsl4.txt"))))))
  (it "parses dsl5"
    (not (nil? (:uuid (parse-dsl (slurp "test/unit/orderdslparser/dsl5.txt"))))))
  (it "parses dsl6"
    (not (nil? (:uuid (parse-dsl (slurp "test/unit/orderdslparser/dsl6.txt")))))))