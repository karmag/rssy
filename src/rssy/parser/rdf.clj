(ns rssy.parser.rdf
  (:require [rssy.data :as data]))

(defn- get-elements [xml key]
  (filter #(= key (:tag %)) (:content xml)))

(defn- get-single-element [xml key]
  (first (get-elements xml key)))

(defn- get-text
  ([xml]
   (let [value (first (:content xml))]
     (when (string? value)
       value)))
  ([xml key]
   (get-text (get-single-element xml key))))

(defn- parse-item [item-xml]
  {:title (get-text item-xml :title)
   :link  (get-text item-xml :link)
   :time  (data/parse-time (get-text item-xml :dc:date))})

(defn- parse-channel [rdf-xml]
  (let [channel-xml (get-single-element rdf-xml :channel)]
    (reduce (fn [channel item-xml]
              (update channel :items conj (parse-item item-xml)))
            {:name    (get-text channel-xml :title)
             :changed (data/parse-time (get-text channel-xml :dc:date))
             :items   []}
            (get-elements rdf-xml :item))))

(defrecord Rdf []
  data/Parser
  (can-parse-xml? [this xml]
    (= :rdf:RDF (:tag xml)))
  (parse-xml [this xml]
    (parse-channel xml)))
