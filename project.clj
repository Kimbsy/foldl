(defproject foldl "0.1.0"
  :dependencies [[org.clojure/clojure "1.12.1"]
                 [com.kimbsy/clunk "2.0.1-SNAPSHOT"]]
  :main ^:skip-aot foldl.core
  :target-path "target/%s"
  ;; @NOTE: un-comment for MacOS
  ;; :jvm-opts ["-XstartOnFirstThread"]
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
