(ns rssy.parser.rss2_0
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
   :time  (data/parse-time (get-text item-xml :pubDate))})

(defn- parse-channel [rss-xml]
  (let [channel-xml (get-single-element rss-xml :channel)]
    (reduce (fn [channel item-xml]
              (update channel :items conj (parse-item item-xml)))
            {:name (get-text channel-xml :title)
             :changed (data/parse-time (or (get-text channel-xml :lastBuildDate)
                                           (get-text channel-xml :pubDate)))
             :items []}
            (get-elements channel-xml :item))))

(defrecord Rss20 []
  data/Parser
  (can-parse-xml? [this xml]
    (and (-> xml :tag (= :rss))
         (-> xml :attrs :version (= "2.0"))))
  (parse-xml [this xml]
    (parse-channel xml)))
