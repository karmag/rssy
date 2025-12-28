(defproject rssy "0"
  :dependencies [[org.clojure/clojure "1.11.1"]

                 ;; database
                 [org.clojure/java.jdbc "0.7.12"]
                 [org.xerial/sqlite-jdbc "3.50.3.0"]

                 ;; gui
                 [seesaw "1.5.0"]
                 [com.formdev/flatlaf "3.6.2"]

                 ;; logging
                 [com.taoensso/telemere "1.1.0"]]
  :profiles {:uberjar {:aot [#".*"]}}
  :jvm-opts ["-Dswing.plaf.metal.controlFont=Arial-18"]
  :repl-options {:init-ns rssy.core}
  :main rssy.core)
