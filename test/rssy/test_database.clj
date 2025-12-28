(ns rssy.test-database
  (:require [clojure.test :refer [deftest is]]
            [rssy.database :as database]))

(deftest test-channel
  (database/with-memory-db
    (let [input {:name "example.com"
                 :link "http://example.com/"
                 :items [{:title "hello"
                          :link "http://example.com/hello"
                          :time 123456}]}]
      (database/add-channel db input)
      (let [all-channels (database/get-channels db)
            channel (first all-channels)]
        (is (= 1 (count all-channels)))
        (is (= (:title channel) (:title input)))
        (is (= (:link channel) (:link input)))
        (let [items (database/get-items db channel)
              item (first items)]
          (is (= 1 (count items)))
          (is (= "hello" (:title item)))
          (is (= "http://example.com/hello" (:link item))))))))
