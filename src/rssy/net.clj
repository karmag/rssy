(ns rssy.net
  (:require [clojure.xml :as xml]
            [taoensso.telemere :as tel])
  (:import java.net.URI
           java.net.http.HttpClient
           java.net.http.HttpClient$Redirect
           java.net.http.HttpRequest
           java.net.http.HttpResponse$BodyHandlers
           java.time.Duration))

(defn- read-body [body]
  (if (string? body)
    body
    (let [bytes (byte-array 2000)]
      (.read body bytes)
      (.trim (String. bytes)))))

(defn get-uri
  "Returns the HTTP response as a map of

  :status-code - HTTP status code
  :headers     - Response headers as a map of keyword to vector of values.
  :body        - Type depends on return argument.
      :stream = Input stream
      :string = String
      :xml    = Clojure XML data structure"
  [uri return]
  (tel/log! {:level :info, :msg (str "Download URI: " uri)})
  (let [client (.. (HttpClient/newBuilder)
                   (followRedirects HttpClient$Redirect/ALWAYS)
                   (build))
        request (.. (HttpRequest/newBuilder)
                    (GET)
                    (uri (URI. uri))
                    (timeout (Duration/ofSeconds 20))
                    (build))
        body-handler (case return
                       (:stream :xml) (HttpResponse$BodyHandlers/ofInputStream)
                       :string (HttpResponse$BodyHandlers/ofString))
        response (.send client request body-handler)
        result {:status-code (.statusCode response)
                :headers (reduce (fn [m [k v]]
                                   (assoc m (keyword k) (vec v)))
                                 {}
                                 (.. response headers map))}]
    (when-not (<= 200 (:status-code result) 299)
      (throw (ex-info "Non-success HTTP response code"
                      {:uri uri
                       :http-response (assoc result
                                             :body (read-body (.body response)))})))
    (assoc result
           :body
           (if (= return :xml)
             (tel/catch->error! {:level :error
                                 :msg "Failed to parse XML"
                                 :data {:uri uri
                                        :http-response result}}
                                (xml/parse (.body response)))
             (.body response)))))
