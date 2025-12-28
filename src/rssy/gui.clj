(ns rssy.gui
  (:require [clojure.pprint :refer [pprint]]
            [rssy.control :as control]
            [rssy.data :as data]
            [seesaw.core :as ss]
            [seesaw.mig :as mig]
            [seesaw.table :as table])
  (:import java.awt.Desktop
           java.net.URI
           javax.swing.event.TableModelEvent
           javax.swing.table.TableModel))

;;--------------------------------------------------
;; helpers

(defn- open-in-browser [uri]
  (.. Desktop getDesktop (browse (URI. uri))))

(def ^:private lock (atom false))

(defmacro ^:private non-blocking-solo
  "Performs the given code only if the lock can be acquired right now.
  Used to ensure that state changes when reacting to events don't
  trigger circular event chains."
  [& body]
  `(when (compare-and-set! lock false true)
     (try ~@body
          (finally
            (reset-vals! lock false)))))

(defn- refresh-group-channel-mapping-table [state]
  (let [channels (->> (control/get-group-channels
                       (:control state)
                       (ss/text (:group|text state)))
                      (sort-by :link)
                      vec)]
    (.setModel (:group|channel-table state)
               (table/table-model :columns [:link :name]
                                  :rows channels))))

(defn- log
  ([state message]
   (log state :info message))
  ([state level message]
   (control/log (:control state) level message)))

;;--------------------------------------------------
;; table model

(defrecord DataTableModel [columns rows model-listeners]
  TableModel
  (addTableModelListener [this listener]
    (swap! model-listeners conj listener))
  (removeTableModelListener [this listener]
    (swap! model-listeners disj listener))
  (getColumnClass [this column-index]
    Object)
  (getColumnCount [this]
    (count columns))
  (getColumnName [this column-index]
    (or (:name (get columns column-index))
        (str (:key (get columns column-index)))))
  (getRowCount [this]
    (count @rows))
  (getValueAt [this row-index column-index]
    (let [col (get columns column-index)]
      ((or (:transform col) identity)
       (get (get @rows row-index) (:key col)))))
  (isCellEditable [this row-index column-index]
    false)
  (setValueAt [this value row-index column-index]
    (throw (UnsupportedOperationException. "setValueAt not supported"))))

(defn- make-table [id columns & extra-args]
  (let [model (DataTableModel. columns (atom []) (atom #{}))
        table (apply ss/table :id id :model model extra-args)]
    (doseq [[col cfg] (map vector
                           (enumeration-seq (.. table getColumnModel getColumns))
                           columns)]
      (when (:width cfg)
        (.setWidth col (:width cfg))
        (.setPreferredWidth col (:width cfg))
        (.setMaxWidth col (* 2 (:width cfg)))))
    table))

(defn- get-data-table-model [obj]
  (if (instance? DataTableModel obj) obj (.getModel obj)))

(defn- update-data-table [obj cmd & args]
  (let [dtm (get-data-table-model obj)
        rows (:rows dtm)
        event
        (case cmd
          :set (do (reset! rows (into [] (first args)))
                   (TableModelEvent. dtm))
          :clear (do (reset! rows [])
                     (TableModelEvent. dtm))
          :add (let [original @rows
                     new-rows (first args)]
                 (swap! rows into new-rows)
                 (TableModelEvent. dtm
                                   (count original)
                                   (+ (count original) (count new-rows))
                                   TableModelEvent/ALL_COLUMNS
                                   TableModelEvent/INSERT))
          :update-at (let [[index f & args] args]
                       (swap! rows update index #(apply f % args))
                       (TableModelEvent. dtm
                                         index
                                         index
                                         TableModelEvent/ALL_COLUMNS
                                         TableModelEvent/UPDATE)))]
    (when event
      (doseq [listener @(:model-listeners dtm)]
        (.tableChanged listener event)))))

(defn- get-data-table-row [obj index]
  (let [rows @(:rows (get-data-table-model obj))]
    (when (< index (count rows))
      (get rows index))))

;;--------------------------------------------------
;; listener intercept

(def ^:private listen-timer (atom 0))

(defn- inspect [result form]
  (cond
    (and (list? form)
         (symbol? (first form))
         (= "control" (namespace (first form))))
    (update result :control-forms #(conj (or % []) form))

    (= 'non-blocking-solo form)
    (assoc result :non-blocking true)

    (or (list? form) (vector? form))
    (reduce inspect result form)

    :else
    result))

(defmacro ^:private ss_listen [component event function]
  (let [line (:line (meta &form))
        comp-form (if (and (list? component)
                           (= 2 (count component))
                           (keyword? (first component))
                           (= 'state (second component)))
                    (first component)
                    component)
        inspect-result (inspect nil function)]
    `(ss/listen ~component ~event
                (fn [& args#]
                  (let [now# (System/currentTimeMillis)]
                    (when (< 500 (- now# @listen-timer))
                      (println "    o---------------------------------------------------------------------"))
                    (reset-vals! listen-timer now#)
                    (println "    | line" ~line "::" '~comp-form "->" '~event)
                    (when (:non-blocking '~inspect-result)
                      (println "    |     non-blocking:" @lock))
                    (doseq [form# (:control-forms '~inspect-result)]
                      (println "    |    " form#)))
                  (apply ~function args#)))))

;;--------------------------------------------------
;; gui layout

(defn- make-view-panel []
  (let [tabbed (ss/tabbed-panel
                :id :view|tabbed-panel
                :placement :top
                :overflow :scroll
                :tabs [{:title "Channels"
                        :content (ss/listbox
                                  :id :view|channels
                                  :model [])}
                       {:title "Groups"
                        :content (ss/listbox
                                  :id :view|groups
                                  :model [])}]
                :minimum-size [300 :by 300])
        item-listing (ss/scrollable
                      (make-table :view|items
                                  [{:key :viewed, :name "New", :width 50, :transform #(if (zero? %) "New" "-")}
                                   {:key :time, :name "Time", :width 150, :transform (comp data/format-time (partial * 1000))}
                                   {:key :source, :width 150, :name "Source"}
                                   {:key :title, :name "Title"}]
                                  :selection-mode :single))]
    (ss/selection! tabbed 1)
    (.. item-listing getVerticalScrollBar (setUnitIncrement 20))
    (mig/mig-panel :constraints ["fill" "[300|grow]"]
                   :items [[tabbed "grow"]
                           [item-listing "grow"]])))

(defn- make-manage-panel []
  (let [channel-buttons (mig/mig-panel
                         :constraints ["fill, insets 0" "[grow 0|grow 0|grow 0|grow|grow 0]"]
                         :items [[(ss/text :id :channels|text) "grow, span, wrap"]
                                 [(ss/button :text "Add" :id :channels|add)]
                                 [(ss/button :text "Remove" :id :channels|remove)]
                                 [(ss/button :text "Refresh" :id :channels|refresh)]
                                 [:fill-h "grow"]
                                 [(ss/button :text "Refresh all" :id :channels|refresh-all)]])

        channel-table (ss/scrollable
                       (make-table :channels|table
                                   [{:key :link}
                                    {:key :name}]
                                   :selection-mode :single))

        channels (mig/mig-panel
                  :constraints ["fill" "" "[grow 0|grow]"]
                  :items [[channel-buttons "grow, wrap"]
                          [channel-table "grow"]]
                  :border "Channels")

        group-buttons (mig/mig-panel
                       :constraints ["fill, insets 0" "[grow 0|grow 0|grow]"]
                       :items [[(ss/text :id :group|text) "grow, span, wrap"]
                               [(ss/button :text "Create" :id :group|create)]
                               [(ss/button :text "Delete" :id :group|delete)]
                               [:fill-h "grow"]])

        group-channels (mig/mig-panel
                        :constraints ["fill, insets 0" "" "[grow 0|grow]"]
                        :items [[(ss/button :text "Add to group"
                                            :id :group|add-channel)
                                 "split 2"]
                                [(ss/button :text "Remove from group"
                                            :id :group|remove-channel)
                                 "wrap"]
                                [(ss/scrollable
                                  (make-table :group|channel-table
                                              [{:key :link}
                                               {:key :name}]
                                              :selection-mode :single))
                                 "grow"]])

        groups (mig/mig-panel
                :constraints ["fill" "" "[grow 0|grow|grow]"]
                :items [[group-buttons "grow, wrap"]
                        [(ss/scrollable
                          (ss/listbox :id :group|list
                                      :model []))
                         "grow, wrap"]
                        [group-channels "grow"]]
                :border "Groups")]

    (mig/mig-panel :constraints ["fill, insets 0" "[grow|grow]"]
                   :items [[channels "grow"]
                           [groups "grow"]])))

(defn- make-log-panel []
  (let [bar (mig/mig-panel
             :constraints ["fill, insets 0" "[grow 0|grow 0|grow]"]
             :items [[(ss/button :text "Clear" :id :log|clear)]
                     [(ss/combobox :id :log|log-level
                                   :model [:debug :info :warn :error])]
                     [:fill-h "grow"]])]
    (mig/mig-panel
     :constraints ["fill" "" "[grow 0|grow|grow]"]
     :items [[bar "grow, wrap"]
             [(ss/scrollable
               (make-table :log|table
                           [{:key :time, :name "Time", :transform data/format-time-precise}
                            {:key :level, :name "Level", :transform name}
                            {:key :msg, :name "Message"}]
                           :selection-mode :single))
              "grow, wrap"]
             [(ss/scrollable
               (ss/text :id :log|details
                        :multi-line? true
                        :editable? false
                        :rows 10))
              "grow"]])))

(defn- make-frame []
  (let [top (mig/mig-panel
             :constraints ["fill, insets n n 0 n" "[grow 0][grow 0][grow 0]20[grow 0][grow]"]
             :items [[(ss/button :text "View" :id :tab|view)]
                     [(ss/button :text "Manage" :id :tab|manage)]
                     [(ss/button :text "Log" :id :tab|log)]
                     [(ss/label :text "..." :id :log|quick) "grow"]])]
    (ss/frame :title "rssy"
              :on-close :exit
              :minimum-size [800 :by 600]
              :content (ss/border-panel
                        :north top
                        :center (ss/card-panel
                                 :id :tab|panel
                                 :items [[(make-view-panel) :view]
                                         [(make-manage-panel) :manage]
                                         [(make-log-panel) :log]])))))

;;--------------------------------------------------
;; listeners

(defn- setup-tab-selection [state]
  (let [set-active (fn [button panel]
                     (ss/show-card! (:tab|panel state) panel)
                     (ss/config! ((juxt :tab|view :tab|manage :tab|log) state)
                                 :enabled? true)
                     (ss/config! (button state) :enabled? false))]
    (set-active :tab|view :view)
    (ss_listen (:tab|view state) :action (fn [_] (set-active :tab|view :view)))
    (ss_listen (:tab|manage state) :action (fn [_] (set-active :tab|manage :manage)))
    (ss_listen (:tab|log state) :action (fn [_] (set-active :tab|log :log)))))

(defn- setup-view-selection [state]
  (ss_listen (:view|tabbed-panel state) :selection
             (fn [_]
               (let [panel (case (:index (ss/selection (:view|tabbed-panel state)))
                             0 (:view|channels state)
                             1 (:view|groups state))
                     selected (ss/selection panel)]
                 (when selected
                   (ss/selection! panel selected)))))

  (let [add-source (fn [items]
                     (let [id->name (->> (control/get-channels (:control state))
                                         (reduce (fn [m ch]
                                                   (assoc m (:id ch) (:name ch)))
                                                 {}))]
                       (map (fn [item]
                              (assoc item :source (id->name (:channel_id item))))
                            items)))]

    (ss_listen (:view|channels state) :selection
               (fn [e]
                 (when-not (.getValueIsAdjusting e)
                   (let [channel-name (ss/selection (:view|channels state))
                         channel (some #(when (= channel-name (:name %)) %)
                                       (control/get-channels (:control state)))]
                     (if (nil? channel)
                       (update-data-table (:view|items state) :set nil)
                       (do (update-data-table (:view|items state)
                                              :set
                                              (add-source
                                               (control/get-items (:control state) channel)))
                           (ss/scroll! (:view|items state) :to :top)))))))

    (ss_listen (:view|groups state) :selection
               (fn [e]
                 (when-not (.getValueIsAdjusting e)
                   (let [group-name (ss/selection (:view|groups state))
                         channels (control/get-group-channels (:control state) group-name)]
                     (if (empty? channels)
                       (update-data-table (:view|items state) :set nil)
                       (do (update-data-table (:view|items state)
                                              :set
                                              (add-source
                                               (control/get-items (:control state) channels)))
                           (ss/scroll! (:view|items state) :to :top))))))))

  (ss_listen (:view|items state) :mouse-clicked
             (fn [e]
               (when (= 2 (.getClickCount e))
                 (let [row (ss/selection e)
                       item (get-data-table-row (:view|items state) row)]
                   (open-in-browser (:link item))
                   (update-data-table (:view|items state) :update-at row assoc :viewed 1)
                   (control/update-item (:control state) item assoc :viewed 1))))))

(defn- setup-channel-management [state]
  (let [set-text
        (fn [text]
          (if (empty? text)
            (ss/text! (:channels|text state) "")
            (ss/text! (:channels|text state) text)))

        find-in-table
        (fn [link]
          (let [link (.trim link)]
            (some (fn [row]
                    (when (= link (.getValueAt (:channels|table state) row 0))
                      row))
                  (range (.. (:channels|table state) getModel getRowCount)))))

        select-in-table
        (fn [link]
          (let [row (find-in-table link)]
            (if (nil? row)
              (.clearSelection (:channels|table state))
              (.setRowSelectionInterval (:channels|table state) row row))))

        adjust-buttons
        (fn []
          (let [text (ss/text (:channels|text state))
                has-text (not-empty text)
                in-table (find-in-table text)]
            (ss/config! (:channels|add state)
                        :enabled? (and has-text (not in-table)))
            (ss/config! ((juxt :channels|remove :channels|refresh) state)
                        :enabled? (and has-text in-table))))]
    (adjust-buttons)
    (ss_listen (:channels|add state) :action
               (fn [_]
                 (control/add-channel (:control state)
                                      (.trim (ss/text (:channels|text state))))))
    (ss_listen (:channels|remove state) :action
               (fn [_]
                 (control/remove-channel (:control state)
                                         (.trim (ss/text (:channels|text state))))))
    (ss_listen (:channels|refresh state) :action
               (fn [_]
                 (future
                   (control/refresh-channels
                    (:control state)
                    [{:link (.trim (ss/text (:channels|text state)))}]))))
    (ss_listen (:channels|refresh-all state) :action
               (fn [_]
                 (future
                   (control/refresh-channels (:control state)))))
    (ss_listen (.getDocument (:channels|text state)) :document
               (fn [_]
                 (non-blocking-solo
                  (let [link (ss/text (:channels|text state))]
                    (select-in-table link)
                    (adjust-buttons)))))
    (ss_listen (:channels|table state) :selection
               (fn [e]
                 (when-not (.getValueIsAdjusting e)
                   (non-blocking-solo
                    (let [index (.getSelectedRow (:channels|table state))]
                      (if (= index -1)
                        (set-text nil)
                        (let [link (.getValueAt (:channels|table state) index 0)]
                          (set-text link)))
                      (adjust-buttons))))))))

(defn- setup-group-management [state]
  (let [select-in-list
        (fn [name]
          (.setSelectedValue (:group|list state) name true)
          (let [value (.getSelectedValue (:group|list state))]
            (when-not (= name value)
              (.clearSelection (:group|list state)))))

        get-table-selection
        (fn [table-key]
          (let [index (.getSelectedRow (table-key state))]
            (when (not= index -1)
              (.getValueAt (table-key state) index 0))))

        find-in-table
        (fn [table-key value]
          (let [value (.trim value)]
            (some (fn [row]
                    (when (= value (.getValueAt (table-key state) row 0))
                      row))
                  (range (.. (table-key state) getModel getRowCount)))))

        adjust-group-buttons
        (fn []
          (let [text (.trim (or (ss/text (:group|text state)) ""))
                has-text (not-empty text)
                in-list (= text (.getSelectedValue (:group|list state)))]
            (ss/config! (:group|create state)
                        :enabled? (and has-text (not in-list)))
            (ss/config! (:group|delete  state)
                        :enabled? (and has-text in-list))))

        adjust-channel-buttons
        (fn []
          (let [input-channel (get-table-selection :channels|table)
                group-selected? (ss/selection (:group|list state))
                has-input-channel? (or (nil? input-channel)
                                       (find-in-table :group|channel-table input-channel))]
            (ss/config! (:group|add-channel state)
                        :enabled? (and input-channel
                                       group-selected?
                                       (not has-input-channel?)))
            (ss/config! (:group|remove-channel state)
                        :enabled? (get-table-selection :group|channel-table))))]
    (adjust-group-buttons)
    (ss_listen (:group|create state) :action
               (fn [_]
                 (control/add-group (:control state)
                                    (ss/text (:group|text state)))))
    (ss_listen (:group|delete state) :action
               (fn [_]
                 (control/remove-group (:control state)
                                       (ss/text (:group|text state)))))

    (ss_listen (.getDocument (:group|text state)) :document
               (fn [_]
                 (non-blocking-solo
                  (let [name (ss/text (:group|text state))]
                    (select-in-list name)
                    (adjust-group-buttons)))))
    (ss_listen (:group|list state) :selection
               (fn [e]
                 (when-not (.getValueIsAdjusting e)
                   (non-blocking-solo
                    (let [value (.getSelectedValue (:group|list state))]
                      (ss/text! (:group|text state) value)
                      (adjust-group-buttons)))
                   (refresh-group-channel-mapping-table state)
                   (adjust-channel-buttons))))

    (adjust-channel-buttons)
    (ss_listen (:group|add-channel state) :action
               (fn [_]
                 (let [group-name (ss/text (:group|text state))
                       channel-link (get-table-selection :channels|table)]
                   (control/add-group-channel-mapping (:control state)
                                                      group-name
                                                      channel-link))))
    (ss_listen (:group|remove-channel state) :action
               (fn [_]
                 (let [group-name (ss/text (:group|text state))
                       channel-link (get-table-selection :group|channel-table)]
                   (control/remove-group-channel-mapping (:control state)
                                                         group-name
                                                         channel-link))))

    (ss_listen (:group|channel-table state) :selection
               (fn [e]
                 (when-not (.getValueIsAdjusting e)
                   (adjust-channel-buttons))))
    (ss_listen (:channels|table state) :selection
               (fn [e]
                 (when-not (.getValueIsAdjusting e)
                   (adjust-channel-buttons))))))

(defn- setup-log-selection [state]
  (ss_listen (:log|table state) :selection
             (fn [e]
               (when-not (.getValueIsAdjusting e)
                 (ss/text! (:log|details state)
                           (binding [*out* (java.io.StringWriter.)]
                             (pprint (get-data-table-row
                                      (:log|table state)
                                      (ss/selection (:log|table state))))
                             (.toString *out*)))))))

(defn- setup-listeners [state]
  (setup-tab-selection state)
  (setup-view-selection state)
  (setup-channel-management state)
  (setup-group-management state)
  (setup-log-selection state))

;;--------------------------------------------------
;; data watchers

(defn- setup-data-watchers [state]
  ;; view - channel-list
  (control/add-channel-listener
   (:control state)
   (fn [_ new]
     (let [model (.getModel (:view|channels state))]
       (.clear model)
       (.ensureCapacity model (count new))
       (doseq [channel (sort-by :name new)]
         (.addElement model (:name channel))))))

  ;; view - group-list
  (control/add-group-listener
   (:control state)
   (fn [_ new]
     (let [model (.getModel (:view|groups state))]
       (.clear model)
       (.ensureCapacity model (count new))
       (doseq [group (sort-by :name new)]
         (.addElement model (:name group))))))

  ;; manage - channel-table
  (control/add-channel-listener
   (:control state)
   (fn [_ new]
     (.setModel (:channels|table state)
                (table/table-model :columns [:link :name]
                                   :rows new))))

  ;; manage - group-list
  (control/add-group-listener
   (:control state)
   (fn [_ new]
     (let [model (.getModel (:group|list state))]
       (.clear model)
       (.ensureCapacity model (count new))
       (doseq [group new]
         (.addElement model (:name group))))))

  ;; manage - group channel-table
  (control/add-group-channel-mapping-listener
   (:control state)
   (fn []
     (refresh-group-channel-mapping-table state)))

  (control/add-log-listener
   (:control state)
   (fn [_ new]
     (ss/text! (:log|quick state) (-> new first :msg))))

  (control/add-log-listener
   (:control state)
   (fn [old new]
     (let [log-entries (take-while #(not= (first old) %) new)]
       (update-data-table (:log|table state) :add log-entries)))))

;;--------------------------------------------------
;; main

(defn- get-state [db root]
  (reduce (fn [m w]
            (if-let [id (ss/id-of w)]
              (do (assert (not (get m id)) "Id exists")
                  (assoc m id w))
              m))
          {:control (control/make db)}
          (ss/select root [:*])))

(defn run [db]
  (let [root (make-frame)
        state (get-state db root)]
    (setup-listeners state)
    (setup-data-watchers state)
    (control/trigger-listeners (:control state))
    (-> root
        (ss/config! :size [1000 :by 800])
        (ss/move! :to [100 100]) ss/show!)
    (log state "rssy started")))
