(ns sc.twitter
   (:require [clj-http.client :as client]
             [cheshire.core :refer :all]
             [clojure.string]
             [net.cgrand.enlive-html :as html]
             [cheshire.core :refer :all]
             [clojure.java.io :as io]
             [clojure.data.csv :as csv]
             [clojure.data.json :as json]))


(def path "/Volumes/ssd/programming/datascience")
(def consumerKey "koQOPIaCJGi5giaq3Saa5dfRE")
(def consumerSecretKey "vDBAV664cVHveP3Kt4Xd0gBICWaevIffdUCLAkHXhgVnUI6Luj ")
(def accessToken "3025319824-Hz6dfmKsxxBGC4rftYmBpVc6zErw7xt9xp9HZQL")
(def accessTokenSecret "RjITYweRXjRgYV3N3fToZj1a0LpwgBZIsmFXbmCk6wOZt")
(def sentimentsFile (str path "/" "AFINN-111.txt"))
(def twitterFile (str path "/" "problem_1_submission.txt"))



(defn read-lines [filename]
  (with-open [rdr (io/reader filename)]
  (doall (line-seq rdr))))


(defn createSentimentsTable [filename]
  (let [
        sentimentsStrings (with-open [in-file (io/reader sentimentsFile)] (doall (csv/read-csv in-file :separator \tab)))
        sentimentsParsed (mapv (fn [x] [(x 0) (read-string (x 1))]) sentimentsStrings)
        sentimentsTable (reduce conj (hash-map) sentimentsParsed)
        ]
  sentimentsTable))

(def sentimentsTable (createSentimentsTable sentimentsFile))

(with-open [in-file (io/reader sentimentsFile)]
  (doall
   (csv/read-csv in-file :separator \tab)))

["abandon" (read-string "-2")]
sentimentsTable

(def twitterLines (vec (read-lines twitterFile)))

(get (json/read-str (twitterLines 16)) "text")

(re-seq #"\w+" (get (json/read-str (twitterLines 12)) "text"))

(re-seq #"\w+" (twitterLines 16))

(defn countSentimentsInLine [twitterLine]
  (let [words (re-seq #"\w+" twitterLine)
        table sentimentsTable
        sentiments (map (fn [x] (get table x 0)) words)]
    (reduce + sentiments)
;;     sentiments
    ))

(reduce + (map #(countSentimentsInLine %) twitterLines))



(defn httpCall [url]
  (client/get url {:socket-timeout 2000 :conn-timeout 2000}))

;; reposters
;; (:body (httpCall "https://api.soundcloud.com/e1/tracks/222501662/reposters?app_version=2d2d934&client_id=02gUJC0hH2ct1EGOcYXQIzRFU91c72Ea&limit=25&linked_partitioning=1&offset=00"))
;;likers
;; (httpCall "https://api-v2.soundcloud.com/tracks/222501662/likers?limit=25&offset=0&linked_partitioning=1&client_id=02gUJC0hH2ct1EGOcYXQIzRFU91c72Ea&app_version=2d2d934")

;; (httpCall "http://sh191485.website.pl/joe/")

;; (get (:content (get (:content (get (:content (get (:content (get (:content (get (:content (get (:content (get (:content (client/parse-html  "http://meteo.waw.pl/")) 1)) 15)) 13)) 3)) 1)) 1)) 1)) 0)


;; ;; helper function checking if error 404 occured


;; (defn got404? [userid method offset] (= (try (client/get (str "http://api.soundcloud.com/users/" userid "/" method "?client_id=" soundcloud_client_id "&limit=100" "&linked_partitioning=1&offset=" offset) {:socket-timeout 2000 :conn-timeout 2000})
;;   (catch Exception e (str (.getMessage e)))) "clj-http: status 404"))


;; fixed function catching 404_errors

;; (defn SCcall100 "http call with choosen method and with client id and linked partitioning set to 100" [userid method offset]
;;   (let [call100 (try (httpCall (str "http://api.soundcloud.com/users/" userid "/" method "?client_id=" soundcloud_client_id "&limit=100" "&linked_partitioning=1&offset=" offset))
;;         (catch Exception e))]
;;     (if (= (:status call100) 200)
;;       call100
;;       (if (got404? userid method offset)
;;         (do (println (str "user doesn't exist!!!" userid)) (spit error_404_list (str userid "\n") :append true))
;;         (loop [retry 1]
;;           (if (< retry 10)
;;             (let [retry_call (try (httpCall (str "http://api.soundcloud.com/users/" userid "/" method "?client_id=" soundcloud_client_id "&limit=100" "&linked_partitioning=1&offset=" offset))
;;                                (catch Exception e))
;;                 _ (println (str userid "_" offset "   will retry, retry: " retry))]
;;               (if (= (:status retry_call) 200)
;;                 retry_call
;;                 (recur (inc retry)))
;;               )))))))






;; (defn -main []
;;   (do (println "enter user id or ids: ")
;;     (let [ids (map read-string (re-seq #"\w+" (read-line)))]
;;       (time (doall (map id_sucker ids))))))


