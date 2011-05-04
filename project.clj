(defproject orderdslparser "1.0.0"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [fnparse "2.2.7"]
                 [matchure "0.10.1"]]
  :dev-dependencies [[com.stuartsierra/lazytest "1.1.2"]
                     [yij/lein-plugins "1.0.2"]]
  :repositories {"stuartsierra-releases" "http://stuartsierra.com/maven2"}
  :lazytest-path ["src" "test/unit"])
