(ns sc.db
  (:import [java.sql SQLException])
  (:require [clojure.java.jdbc :as jdbc]))


(def db {:classname "com.mysql.jdbc.Driver"
         :subprotocol "mysql"
         :subname "//localhost:3306/soundcloud"
         :user "root"
         :password "pass"})


(defn retryExecute [query n error]
  (loop [i 0]
    (if (> i n)
      (println "failed retryExecute error code: " error)
      (do
        (Thread/sleep 500)
        (try
          (jdbc/execute! db [query])
          (catch java.sql.SQLException e))
        (recur (inc i))))))


(defn insertIgnoreGraph
  "bulk insert of values [user follows] in vector xs into graph table, ignores duplicates"
  [xs]
  (let [strings (map #(str "(" (get % 0) "," (get % 1) ")") xs)
        stringCall (reduce #(str %1 "," %2) strings)
        query (str "INSERT IGNORE INTO `graph` (`user`, `follows`) VALUES " stringCall ";")]
    (try
      (jdbc/execute! db [query])
      (catch java.sql.SQLException e
        (retryExecute query 5 (.getErrorCode e))))))


(defn insertIgnoreUsers
  "bulk insert of values
  [id url fake name country email followers followings tracks likes reposts desl plan]
  in vector xs into users table, ignores duplicates"
  [xs]
  (let [strings (map #(str "(" (get % 0) ",'" (get % 1) "'," (get % 2) ",'" (get % 3) "','" (get % 4)
                           "','" (get % 5) "'," (get % 6) "," (get % 7) "," (get % 8) "," (get % 9)
                           "," (get % 10) "," (get % 11)  ",'" (get % 12) "')") xs)
        stringCall (reduce #(str %1 "," %2) strings)
        query (str "INSERT IGNORE INTO `users` (`id`, `url`, `fake`, `name`, `country`, `email`, `followers`, `followings`, `tracks`, `likes`, `reposts`, `desl`, `plan`) VALUES " stringCall ";")]
    (try
      (jdbc/execute! db [query])
      (catch java.sql.SQLException e
        (retryExecute query 5 (.getErrorCode e))))))


(defn insertSavedFollower "insert user whose followers are already saved" [id]
  (let [query (str "INSERT IGNORE INTO `savedfollowers` (`id`) VALUES (" id ");")]
    (jdbc/execute! db [query])))


(defn insertSavedFollowing "insert user whose followings are already saved" [id]
  (let [query (str "INSERT IGNORE INTO `savedfollowings` (`id`) VALUES (" id ");")]
    (jdbc/execute! db [query])))

(defn insert404 "insert non existing user ids" [id]
  (let [query (str "INSERT IGNORE INTO `e404` (`id`) VALUES (" id ");")]
    (jdbc/execute! db [query])))


(defn notSavedFollowings [id]
  (empty? (jdbc/query db [(str "SELECT * FROM `e404` JOIN `savedfollowings` where e404.id="
                               id " or savedfollowings.id=" id ";")])))

(defn notSavedFollowers [id]
  (empty? (jdbc/query db [(str "SELECT * FROM `e404` JOIN `savedfollowers` where e404.id="
                               id " or savedfollowers.id=" id ";")])))

(insert404 7)

(notSavedFollowers 1)
