(ns sc.tryouts
   (:require [clj-http.client :as client]
             [cheshire.core :refer :all]
             [clojure.string]
             [net.cgrand.enlive-html :as html]
             [clojure.java.io :as io]
             [clojure.edn :as edn]
             [sc.inits :refer :all]
             ))




(defn nameContainsEdn [file]
  (.contains (str file) "edn"))


(defn getEdnName ".iofile -> string" [file directory]
  (if (nameContainsEdn file)
    (let [length (count (str directory))
          filename (str file)
          index (.indexOf filename ".edn")]
      (subs filename length index))))


(defn getUserIdsFromFolder "returns list of .edn files" [dirPath]
  (let [directory (clojure.java.io/file dirPath)]
    (filter #(not= % nil) (map (fn [x] (getEdnName x dirPath)) (file-seq directory)))))

(defn httpCall [url]
  (client/get url {:socket-timeout 2000 :conn-timeout 2000}))

;; reposters
(:body (httpCall "https://api.soundcloud.com/e1/tracks/222501662/reposters?app_version=2d2d934&client_id=02gUJC0hH2ct1EGOcYXQIzRFU91c72Ea&limit=25&linked_partitioning=1&offset=00"))

(:body (httpCall "https://api.soundcloud.com/e1/tracks/222501662/reposters?app_version=2d2d934&client_id=af3e5e31e2e63ddad94791906ebddaec&limit=25&linked_partitioning=1&offset=00"))




;;likers

(def track_Id 231898794)
(httpCall "https://api-v2.soundcloud.com/tracks/231898794/likers?limit=25&offset=0&linked_partitioning=1&client_id=02gUJC0hH2ct1EGOcYXQIzRFU91c72Ea&app_version=2d2d934")
(httpCall (str "http://api-v2.soundcloud.com/tracks/" track_Id "/likers?client_id=" soundcloud_client_id "&limit=100"))
;; (httpCall "http://sh191485.website.pl/joe/")

;; notifications

;;userLikes
(httpCall "https://api-v2.soundcloud.com/users/107508374/likes?limit=10&offset=0&linked_partitioning=1&client_id=02gUJC0hH2ct1EGOcYXQIzRFU91c72Ea&app_version=07a58eb")

