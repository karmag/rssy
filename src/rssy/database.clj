(ns rssy.database
  (:require [clojure.java.jdbc :as jdbc]))

(defn setup-tables [db]
  (jdbc/db-do-commands
   db
   [(jdbc/create-table-ddl :channel
                           [[:id :integer :primary :key :autoincrement]
                            [:name :text]
                            [:link :text :not :null]
                            [:changed :int]]
                           {:conditional? true})
    (jdbc/create-table-ddl :item
                           [[:id :integer :primary :key :autoincrement]
                            [:channel_id :int :not :null]
                            [:title :text]
                            [:link :text]
                            [:time :int :not :null :default "(unixepoch())"]
                            [:viewed :bool :not :null :default 0]]
                           {:conditional? true})
    (jdbc/create-table-ddl :usergroup
                           [[:id :integer :primary :key :autoincrement]
                            [:name :text :not :null]]
                           {:conditional? true})
    (jdbc/create-table-ddl :group_channel
                           [[:group_id :integer :not :null]
                            [:channel_id :integer :not :null]
                            [:primary :key "(group_id, channel_id)"]]
                           {:conditional? true})]))

(defmacro with-db [& body]
  `(jdbc/with-db-connection [~'db {:classname "org.sqlite.JDBC"
                                   :subprotocol "sqlite"
                                   :subname "rssy.sqlite"}]
     (setup-tables ~'db)
     ~@body))

(defmacro with-memory-db [& body]
  `(jdbc/with-db-connection [~'db {:classname "org.sqlite.JDBC"
                                   :subprotocol "sqlite"
                                   :subname ":memory:"}]
     (setup-tables ~'db)
     ~@body))

(defn- get-channel-id [db channel]
  (->> (jdbc/query db ["SELECT id FROM channel WHERE link=?"
                       (:link channel)])
       first
       :id))

(defn- add-item
  "Adds the item if it doesn't exist. Returns true if the item was
  added."
  [db channel-id item]
  (let [found (not-empty (jdbc/query
                          db
                          ["SELECT * FROM item WHERE channel_id=? AND title=? AND link=?"
                           channel-id
                           (:title item)
                           (:link item)]))
        item (cond-> (assoc item :channel_id channel-id)
               (nil? (:time item)) (dissoc :time))]
    (if found
      false
      (do (jdbc/insert! db :item item)
          true))))

(defn add-channel
  "Adds the channel if it doesn't exist. Then adds all items in the
  channel."
  [db channel]
  (let [channel-id (get-channel-id db channel)]
    (when-not channel-id
      (jdbc/insert! db :channel (dissoc channel :items)))
    (let [channel-id (or channel-id (get-channel-id db channel))]
      (doseq [item (:items channel)]
        (add-item db channel-id item)))))

(defn remove-channel [db channel]
  (let [id (get-channel-id db channel)]
    (jdbc/with-db-transaction [conn db {::jdbc/auto-commit? true}]
      (jdbc/delete! conn :item ["channel_id=?" id])
      (jdbc/delete! conn :group_channel ["channel_id=?" id])
      (jdbc/delete! conn :channel ["id=?" id]))))

(defn get-channels
  "Fetches all channels."
  [db]
  (jdbc/query db ["SELECT * FROM channel"]))

(defn get-items
  "Returns the most current items from the given channels. channels is
  either a channel, a sequence of channels or nil."
  [db channels & {:keys [limit] :or {limit 10}}]
  (let [channels (if (map? channels) [channels] channels)
        all-channels? (empty? channels)
        query_part (->> (repeat (count channels) "channel_id=? ")
                        (interpose "OR ")
                        (apply str))]
    (jdbc/query db (apply vector
                          (str "SELECT * FROM item "
                               (if all-channels?
                                 ""
                                 (str "WHERE " query_part))
                               "ORDER BY time DESC "
                               "LIMIT ? ")
                          (concat (map :id channels)
                                  [limit])))))

(defn update-item
  "Updates the item by (apply f item args). Returns the updated item."
  [db item f & args]
  (let [old (first (jdbc/query db ["SELECT * FROM item WHERE id=?"
                                   (:id item)]))]
    (assert old)
    (let [new (apply f old args)]
      (jdbc/update! db :item new ["id=?" (:id item)])
      new)))

(defn- get-group-id [db group]
  (-> (jdbc/query db ["SELECT id FROM usergroup WHERE name=?" (:name group)])
      first
      :id))

(defn add-group [db group]
  (when-not (get-group-id db group)
    (jdbc/insert! db :usergroup {:name (:name group)})))

(defn get-groups [db]
  (->> (jdbc/query db ["SELECT * FROM usergroup"])
       (map (fn [group]
              (let [channels (->> (jdbc/query db ["SELECT channel_id FROM group_channel WHERE group_id=?"
                                                  (:id group)])
                                  (map :channel_id)
                                  set)]
                (assoc group :channel-ids channels))))
       doall))

(defn remove-group [db group]
  (let [id (get-group-id db group)]
    (jdbc/with-db-transaction [conn db {::jdbc/auto-commit? true}]
      (jdbc/delete! conn :group_channel ["group_id=?" id])
      (jdbc/delete! conn :usergroup ["id=?" id]))))

(defn add-group-channel-mapping [db group channel]
  (let [group-id (get-group-id db group)
        channel-id (get-channel-id db channel)]
    (jdbc/insert! db :group_channel {:group_id group-id
                                     :channel_id channel-id})))

(defn remove-group-channel-mapping [db group channel]
  (let [group-id (get-group-id db group)
        channel-id (get-channel-id db channel)]
    (jdbc/delete! db :group_channel ["group_id=? AND channel_id=?"
                                     group-id
                                     channel-id])))

(defn debug-print [db table]
  (doseq [row (jdbc/query db
                          (str "SELECT * FROM " (name table))
                          {:as-arrays? true})]
    (prn row)))
