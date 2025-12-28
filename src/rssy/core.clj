(ns rssy.core
  (:require [rssy.gui :as gui]
            [rssy.database :as database]
            [taoensso.telemere :as tel])
  (:import com.formdev.flatlaf.FlatLaf
           com.formdev.flatlaf.FlatDarkLaf)
  (:gen-class))

(defn -main []
  (tel/set-min-level! nil)

  (database/with-db
    (FlatLaf/registerCustomDefaultsSource "themes")
    (FlatDarkLaf/setup)

    (gui/run db)
    (Thread/sleep Long/MAX_VALUE)))
