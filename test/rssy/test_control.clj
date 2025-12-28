(ns rssy.test-control
  (:require [clojure.test :refer :all]
            [rssy.control :as control]
            [rssy.database :as database])
  (:import (com.sun.net.httpserver HttpHandler
                                   HttpExchange
                                   HttpServer)
           java.net.InetSocketAddress))

(defn- simple-server [response-atom]
  (let [server (HttpServer/create (InetSocketAddress. 5678) 0)]
    (.createContext server "/"
                    (proxy [HttpHandler] []
                      (handle [^HttpExchange exchange]
                        (let [response (.getBytes (first @response-atom))]
                          (swap! response-atom subvec 1)
                          (.read (.getRequestBody exchange))
                          (.sendResponseHeaders exchange 200 (count response))
                          (doto (.getResponseBody exchange)
                            (.write response)
                            (.close))))))
    (.setExecutor server nil)
    (.start server)
    server))

(defmacro ^:private with-control [& body]
  `(database/with-memory-db
     (let [~'control (control/make ~'db)
           response# (atom [])
           server# (simple-server response#)
           ~'add-server-response (fn [text#] (swap! response# conj text#))]
       (try ~@body
            (finally
              (.stop server# 0))))))

(def rss20
  "<rss version=\"2.0\">
    <channel>
        <title>News and stuff</title>
        <link>https://example.com/</link>
        <description>Thingamajiggz and stuff</description>
        <item><title>Alpha</title><link>https://alpha</link><pubDate>Fri, 03 Oct 2025 04:21:23 +0000</pubDate></item>
        <item><title>Beta</title><link>https://beta</link><pubDate>Fri, 03 Oct 2025 12:52:00 +0000</pubDate></item>
        <item><title>Gamma</title><link>https://gamma</link></item>
    </channel>
</rss>")

(deftest test-add-channel
  (with-control
    (add-server-response rss20)
    (control/add-channel control "http://localhost:5678")
    (let [all-channels (control/get-channels control)
          channel (first all-channels)]
      (is (= 1 (count all-channels)))
      (is (= (:name channel) "News and stuff"))
      (is (= (:link channel) "http://localhost:5678")))
    (let [all-items (control/get-items control)]
      (is (= 3 (count all-items)))
      (is (= #{"Alpha" "Beta" "Gamma"}
             (set (map :title all-items))))
      (doseq [item all-items]
        (is (number? (:time item)))))))
