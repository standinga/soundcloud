(ns sc.gettersNew
  (:import [java.sql SQLException])
  (:require [clj-http.client :as client]
            [cheshire.core :refer :all]
            [clojure.string]
            [clojure.edn :as edn]
            [sc.inits :refer :all]
            [clojure.java.jdbc :as jdbc]
            [sc.emails :as emails]
            [sc.db :as db]
            ))


(def baseUrl "http://api.soundcloud.com/users/")

(def followersString "/followers?client_id=af3e5e31e2e63ddad94791906ebddaec&page_size=200")

(def followingsString "/followings?client_id=af3e5e31e2e63ddad94791906ebddaec&page_size=200")

(def userString "?client_id=af3e5e31e2e63ddad94791906ebddaec")

(def gra 23955110)

(def bou 990322)

(def boj 107508374)

(defn httpCall [url]
  (client/get url {:socket-timeout 2000 :conn-timeout 2000}))

(defn user_map "function converts string ids to key ids" [user]
  {:id (user "id"), :username (user "username"),
   :followers_count (user "followers_count"), :country (user "country"), :full_name (user "full_name"),
   :track_count (user "track_count") :plan (user "plan"), :followings_count (user "followings_count"),
   :description (user "description"), :reposts_count (user "reposts_count"),
   :likes_count (user "likes_count"), :permalink (user "permalink")
   })


(defn escapeChars [string]
  (if (> (count string) 40) "NULL"
    (if (= string nil) "NULL"
      (-> string
          (clojure.string/replace "\\" "")
          (clojure.string/replace "'" "\\'")))))


(defn userVector [userData]
  (let [mappedUser (user_map userData)
        id (get mappedUser :id)
        userName (escapeChars (get mappedUser :username))
        countryRuff (if (not= (get mappedUser :country) nil) (get mappedUser :country) "NULL")
        country (escapeChars countryRuff)
        followers (get mappedUser :followers_count)
        followings (get mappedUser :followings_count)
        reposts (get mappedUser :reposts_count)
        likes (get mappedUser :likes_count)
        tracks (get mappedUser :track_count)
        url (get mappedUser :permalink)
        desl (count (get mappedUser :description))
        fake 0
        plan (if (= (get mappedUser :plan) "Pro Unlimited") "U" (if  (= (get mappedUser :plan) "Pro") "P" "F"))
        emailRuff (emails/extractEmailFromDescription (get mappedUser :description))
        email (escapeChars emailRuff)]
    {:id id :userData [id url fake userName country email followers followings tracks 0 0 desl plan]}))


(defn httpCallAndRetry "http call with choosen url will try to retry " [url]
  (let [call100 (try (httpCall url) (catch Exception e))]
    (if (= (:status call100) 200)
      call100
      (if (= (:status call100) 404)
        (println (str "user doesn't exist!!!" url))
        (loop [retry 1]
          (if (< retry 20)
            (let [retry_call (try (httpCall url) (catch Exception e))]
              (if (= (:status retry_call) 200)
                retry_call
                (recur (inc retry))))))))))


(defn idToUrl [id]
  (str baseUrl id followersString))


(defn urlCall [urlIn]
  (loop [url urlIn i 0 acc []]
    (if (and (not= url nil) (< i 400000))
      (let [collection (get (parse-string (:body (httpCallAndRetry url)))"collection")
            newUrl (get (parse-string (:body (httpCallAndRetry url)))"next_href")
            _ (println (str i "  " url "   " (count collection)))]
        (recur newUrl (inc i) (into acc collection)))
      acc)))


(defn saveToDB [acc userId]
  (let [rawData (mapv userVector acc)
        userData (mapv #(get % :userData) rawData)
        userIDS (mapv (fn [x] [(get x :id) userId]) rawData)
        _ (print "*")]
    (try (db/insertIgnoreUsers userData) (catch Exception e (println e)))
    (try (db/insertIgnoreGraph userIDS) (catch Exception e (println e)))))


(defn getUserInfo [userInfo]
  (-> (get userInfo :body)
      parse-string
      userVector
      :userData))

;; (-> (str baseUrl 1 userString)
;;     httpCallAndRetry
;;     getUserInfo)


;; d/l followers or followings and isert into db every n-dloaded users

(defn dloadAndSave [userId]
  (if (db/notSavedFollowers userId)
    (let [userURL (str baseUrl userId followersString)]
      (loop [url userURL i 0 acc []]
        (if (and (not= url nil) (< i 400000))
          (let [collection (get (parse-string (:body (httpCallAndRetry url)))"collection")
                newUrl (get (parse-string (:body (httpCallAndRetry url)))"next_href")
                _ (if (= (mod i 20) 0) (println (str i "  " url "   " (count collection))))]
            (if (> (count acc) 1000)
              (do (saveToDB (into acc collection) userId) (recur newUrl (inc i) []))
              (recur newUrl (inc i) (into acc collection))))
          (saveToDB acc  userId))))
    (println "already saved followers")))


(defn dloadAndSave_Followers [userId]
  (if (db/notSavedFollowers userId) ;check if already in or not existing
    (let [userInfo (httpCallAndRetry (str baseUrl userId userString))]
      (if (not= userInfo nil) ;if nil it means error 404
        (let [userURL (str baseUrl userId followersString)]
          (loop [url userURL i 0]
            (if (not= url nil)
              (let [collection (get (parse-string (:body (httpCallAndRetry url)))"collection")
                    newUrl (get (parse-string (:body (httpCallAndRetry url)))"next_href")]
                (do
                  (db/insertIgnoreUsers [(getUserInfo userInfo)])
                  (if (not= collection [])
                    (saveToDB collection userId))
                  (recur newUrl (inc i))))
              (do
                (db/insertSavedFollower userId)
                (print (str userId " - "))))))
        (db/insert404 userId)))
    (print " a ")))




;;(def test1 (str baseUrl 147331031 followersString))

;(db/insertIgnoreUsers
;; (def data (mapv #(get % :userData) (mapv userVector (urlCall (idToUrl 37791204)))))

;; (def data (urlCall (idToUrl 25406)))





;)
;; (spit "/Users/michal/repos/data.edn" (with-out-str (pr data)))

;; (subvec (read-string (slurp "/Users/michal/repos/data.edn")) 6925 6926)

;; (urlCall (idToUrl 1600769))


;; (subvec (read-string (slurp "/Users/michal/repos/data.edn")) 15289 15290)

;;  (db/insertIgnoreUsers (subvec (read-string (slurp "/Users/michal/repos/data.edn")) 15289 15290))
;; (def err (read-string (slurp "/Users/michal/repos/data.edn")) )
;; (count err)


(defn errorNotFound [xs]
  (try
    (db/insertIgnoreUsers (mapv #(get % :userData) (mapv userVector xs)))
    (catch SQLException e#
      nil)))


(defn findBadQuerry
  "this function perforns binary search through vector of queries
  trying inserting then into db
  if there is bad syntax querry it will be returned"
  [xs]
  (println "find bad")
  (if (not= xs [])
    (if (not (errorNotFound xs))
      (let [length (count xs)]
        (if (> length 2)
          (let [half (/ length 2)
                L (subvec xs 0 half)
                R (subvec xs half)]
            (findBadQuerry L)
            (findBadQuerry R))
          (println (get (userVector (get xs 0)) :userData)))))))


(defn testQuery [xs]
  (doall (map findBadQuerry (map vec (partition-all 500 xs)))))


(defn insertToDB [userId]
  (let [userURL (str "http://api.soundcloud.com/users/" userId "/followers?client_id=af3e5e31e2e63ddad94791906ebddaec&page_size=200")
        rawData (mapv userVector (urlCall userURL))
        userData (mapv #(get % :userData) rawData)
        userIDS (mapv (fn [x] [(get x :id) userId]) rawData)]
    (db/insertIgnoreUsers userData)
    (db/insertIgnoreGraph userIDS)))
;(dloadAndSave 186022746)

;; (time (doall (map dloadAndSave_Followers  (range 3 4))))

;; (time (doall (pmap (fn [x] (doall (pmap dloadAndSave_Followers x))) (partition-all 20 (range 4600 5000)))))


(defn get_user_id "returns user id from user name" [user_name]
  "takes name of user and returns user id"
  (let [call (httpCall (str "http://api.soundcloud.com/users/" user_name "?client_id=" soundcloud_client_id))
        parsed_call (parse-string (:body call))]
    (get parsed_call "id")))


(defn -main []
  (do
    (println "enter ids range: ")
    (let
      [ids (map read-string (re-seq #"\w+" (read-line)))
       a (first ids)
       b (second ids)]
      (time (doall (pmap (fn [x] (doall (pmap dloadAndSave_Followers x))) (partition-all 20 (range a b))))))))

(-main)
