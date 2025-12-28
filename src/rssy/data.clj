(ns rssy.data
  "Common data types and utility functions."
  (:import java.text.ParseException
           java.text.SimpleDateFormat
           java.util.Date
           java.util.Locale))

(def ^:private rfc-822-datetime (SimpleDateFormat. "EEE, dd MMM yyyy HH:mm:ss z" Locale/ENGLISH))
(def ^:private iso-8601-datetime (SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ssX"))
(def ^:private simple-date (SimpleDateFormat. "yyyy-MM-dd"))

(def ^:private human-readable-time (SimpleDateFormat. "MMM dd, HH:mm" Locale/ENGLISH))
(def ^:private human-readable-time-precise (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss" Locale/ENGLISH))

(defn parse-time
  "Parses an rfc822 or iso8601 datetime and returns it as unix time (seconds).
  Return nil if the time can not be parsed."
  [time-text]
  (when time-text
    (or (try (let [date (.parse rfc-822-datetime time-text)]
               (long (/ (.getTime date) 1000)))
             (catch ParseException _))
        (try (let [date (.parse iso-8601-datetime time-text)]
               (long (/ (.getTime date) 1000)))
             (catch ParseException _))
        (try (let [date (.parse simple-date time-text)]
               (long (/ (.getTime date) 1000)))
             (catch ParseException _)))))

(defn format-time [milliseconds]
  (.format human-readable-time (Date. (long milliseconds))))

(defn format-time-precise [milliseconds]
  (.format human-readable-time-precise (Date. (long milliseconds))))

(defprotocol Parser
  (can-parse-xml? [this xml])
  (parse-xml [this xml]))
