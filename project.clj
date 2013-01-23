(defproject proofpeer-logic "0.1.0-SNAPSHOT"
  :description "ProofPeer Logic"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.json "0.1.2"]
                 [the/parsatron "0.0.2-phlegmaticprogrammer"]
                 [phlegmaticprogrammer/clojure_util "0.1.1"]
                 [slingshot "0.10.2"]
                 ]
  :dev-dependencies [[lein-autodoc "0.9.0"]]
  :test-path "src"
  :aot :all
  :plugins [[lein-swank "1.4.4"]])