(defproject risco_de_ectasia "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [seesaw "1.4.5"]]
  :main ^:skip-aot risco-de-ectasia.core
  :resource-paths ["lib/weka.jar"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
