(ns rssy.parser.atom
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
   :link  (-> (get-single-element item-xml :link) :attrs :href)
   :time  (or (data/parse-time (get-text item-xml :published))
              (data/parse-time (get-text item-xml :updated)))})

(defn- parse-channel [feed-xml]
  (reduce (fn [channel item-xml]
            (update channel :items conj (parse-item item-xml)))
          {:name (get-text feed-xml :title)
           :changed (or (data/parse-time (get-text feed-xml :published))
                        (data/parse-time (get-text feed-xml :updated)))
           :items []}
          (get-elements feed-xml :entry)))

(defrecord Atom []
  data/Parser
  (can-parse-xml? [this xml]
    (println (:tag xml) (-> xml :attrs :xmlns))
    (and (= :feed (:tag xml))
         (-> xml :attrs :xmlns (= "http://www.w3.org/2005/Atom"))))
  (parse-xml [this xml]
    (parse-channel xml)))
