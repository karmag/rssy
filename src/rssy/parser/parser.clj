(ns rssy.parser.parser
  (:require [rssy.data]
            [rssy.parser.atom]
            [rssy.parser.rss2_0]
            [rssy.parser.rdf]))

(def ^:private parsers [(rssy.parser.atom/->Atom)
                        (rssy.parser.rss2_0/->Rss20)
                        (rssy.parser.rdf/->Rdf)])

(defn parse-xml
  "Parses clojure XML data into channel data. Attaches the uri as :link
  to the returned channel."
  [xml uri]
  (let [available (filter #(rssy.data/can-parse-xml? % xml)
                          parsers)
        parser (first available)]
    (when (empty? available)
      (throw (ex-info "No parser for content" {})))
    (assoc (rssy.data/parse-xml parser xml)
           :link uri)))
