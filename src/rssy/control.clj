(ns rssy.control
  "Higher level controls for the internals of the application."
  (:require [rssy.database :as database]
            [rssy.parser.parser :as parser]
            [rssy.net :as net]
            [taoensso.telemere :as tel]))

(defn- event-channels-changed [control]
  (dosync
   (ref-set (::channels control)
            (->> (database/get-channels (::db control))
                 (sort-by :link)
                 vec))))

(defn- event-groups-changed [control]
  (dosync
   (ref-set (::groups control)
            (->> (database/get-groups (::db control))
                 (sort-by :name)
                 vec))))

(defn- event-items-changed [control]
  (doseq [listener @(::item-listeners control)]
    (try (listener)
         (catch Exception e
           (tel/error! {:level :error
                        :msg "Item listener failed"
                        :data {:listener listener}
                        :error e})))))

(defn- event-group-channel-mapping-changed [control]
  (doseq [listener @(::group-channel-mapping-listeners control)]
    (try (listener)
         (catch Exception e
           (tel/error! {:level :error
                        :msg "Group-channel mapping listener failed"
                        :data {:listener listener}
                        :error e})))))

(defn make [db]
  (let [control {::db db
                 ::channels (ref [])
                 ::groups (ref [])
                 ::log (atom nil)
                 ::channel-listeners (atom #{})
                 ::group-listeners (atom #{})
                 ::item-listeners (atom #{})
                 ::group-channel-mapping-listeners (atom #{})
                 ::log-listeners (atom #{})}]
    (add-watch (::channels control) nil
               (fn [_ _ old new]
                 (doseq [listener @(::channel-listeners control)]
                   (try (listener old new)
                        (catch Exception e ;; TODO logging
                          (println "Calling channel listener failed" e))))))
    (add-watch (::groups control) nil
               (fn [_ _ old new]
                 (doseq [listener @(::group-listeners control)]
                   (try (listener old new)
                        (catch Exception e ;; TODO logging
                          (println "Calling group listener failed" e))))))
    (add-watch (::log control) nil
               (fn [_ _ old new]
                 (doseq [listener @(::log-listeners control)]
                   (try (listener old new)
                        (catch Exception e ;; TODO logging
                          (println "Calling log listener failed" e))))))
    control))

(defn log
  ([control kvs]
   (let [kvs (merge {:time (System/currentTimeMillis)
                     :level :info}
                    kvs)]
     (swap! (::log control) conj kvs)
     (tel/log! kvs)))
  ([control level message]
   (log control {:level level :msg message})))

(defn add-channel-listener [control listener]
  (swap! (::channel-listeners control) conj listener))

(defn add-group-listener [control listener]
  (swap! (::group-listeners control) conj listener))

(defn add-item-listener [control listener]
  (swap! (::item-listeners control) conj listener))

(defn add-group-channel-mapping-listener [control listener]
  (swap! (::group-channel-mapping-listeners control) conj listener))

(defn add-log-listener [control listener]
  (swap! (::log-listeners control) conj listener))

(defn trigger-listeners [control]
  (event-channels-changed control)
  (event-groups-changed control)
  (event-items-changed control)
  (event-group-channel-mapping-changed control))

(defn add-channel [control uri]
  (tel/catch->error! {:level :error
                      :msg "Failed to add channel"
                      :data {:uri uri}}
                     (database/add-channel (::db control)
                                           (-> (net/get-uri uri :xml)
                                               :body
                                               (parser/parse-xml uri))))
  (tel/log! {:level :info, :msg (str "Added channel " uri)})
  (event-channels-changed control)
  (event-items-changed control))

(defn remove-channel [control uri]
  (if-let [channel (some #(when (= uri (:link %)) %) @(::channels control))]
    (do (database/remove-channel (::db control) channel)
        (tel/log! {:level :info, :msg (str "Removed channel " uri), :data channel})
        (event-channels-changed control)
        (event-items-changed control))
    (tel/log! {:level :error
               :msg (str "Removing unknown channel " uri)
               :data {:uri uri
                      :existing-uris (sort (map :link (::channels control)))}})))

(defn get-channels [control]
  (database/get-channels (::db control)))

(defmacro ^:private run-check
  "Execute the body. On exception catch and raise a clojure exception
  with the given exception-data."
  [exception-data & body]
  `(try ~@body
        (catch Exception e#
          (let [data# ~exception-data]
            (throw (ex-info (or (:message data#) (.getMessage e#))
                            data#
                            e#))))))

(defn refresh-channels
  ([control]
   (refresh-channels control (get-channels control)))
  ([control channels]
   (log control :info (if (= 1 (count channels))
                        (format "Refreshing %s" (-> channels first :link))
                        (format "Refreshing %d channels" (count channels))))

   (let [error-counter (atom 0)]
     (doseq [[index ch] (map-indexed vector channels)]
       (try (let [link (:link ch)
                  http-response (run-check {:message "Download failed"
                                            :link link}
                                           (net/get-uri link :xml))
                  new-channel (run-check {:message "Parsing failed"
                                          :link link
                                          :body (:body http-response)}
                                         (parser/parse-xml (:body http-response)
                                                           link))]
              (run-check {:message "Database failed"
                          :link link
                          :data new-channel}
                         (database/add-channel (::db control) new-channel))
              (log control :info (format "[%d/%d] OK %s"
                                         (inc index)
                                         (count channels)
                                         (:link ch))))
            (catch Exception e
              (swap! error-counter inc)
              (log control {:level :error
                            :msg (format "[%d/%d] FAILED %s"
                                         (inc index)
                                         (count channels)
                                         (:link ch))
                            :data ch
                            :error e}))))

     (log control :info (format "Refreshed %d channels (%d failed)"
                                (count channels)
                                @error-counter))
     (event-items-changed control))))

(defn get-items
  ([control]
   (get-items control nil))
  ([control channels]
   (database/get-items (::db control) channels {:limit 50})))

(defn update-item [control item f & args]
  (tel/catch->error!
   {:level :error
    :msg "Failed to update item"
    :data {:item item, :f f, :args args}}
   (let [item (apply database/update-item (::db control) item f args)]
     (event-items-changed control)
     item)))

(defn add-group [control group-name]
  (database/add-group (::db control) {:name group-name})
  (event-groups-changed control)
  (tel/log! {:level :info, :msg "Added group" :data {:group group-name}}))

(defn remove-group [control group-name]
  (database/remove-group (::db control) {:name group-name})
  (event-groups-changed control)
  (event-group-channel-mapping-changed control)
  (tel/log! {:level :info, :msg "Removed group" :data {:group group-name}}))

(defn add-group-channel-mapping [control group-name channel-link]
  (database/add-group-channel-mapping (::db control)
                                      {:name group-name}
                                      {:link channel-link})
  (event-group-channel-mapping-changed control)
  (log control  {:level :info
                 :msg "Adding channel to group"
                 :data {:group group-name :channel channel-link}}))

(defn remove-group-channel-mapping [control group-name channel-link]
  (database/remove-group-channel-mapping (::db control)
                                         {:name group-name}
                                         {:link channel-link})
  (event-group-channel-mapping-changed control)
  (log control  {:level :info
                 :msg "Removing channel from group"
                 :data {:group group-name :channel channel-link}}))

(defn get-group-channels [control group-name]
  (let [all-groups (database/get-groups (::db control))
        all-channels @(::channels control)
        group (first (filter #(= group-name (:name %)) all-groups))
        channel-ids (set (:channel-ids group))]
    (filter #(channel-ids (:id %)) all-channels)))
