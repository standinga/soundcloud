(ns sc.getters
   (:require [clj-http.client :as client]
             [cheshire.core :refer :all]
             [clojure.string]
             [net.cgrand.enlive-html :as html]
             [clojure.java.io :as io]
             [clojure.edn :as edn]
             [sc.inits :refer :all]
             [sc.savers :as savers]
             ))


;; !!!!!
;; to run those functions you need enter below sound_client_id::::

(def error_counter (atom 1)) ; initialize error counter to monitor how many d/loads were wrong

(def howmanyleft (atom 0))   ; initialize to zero how many left to download

(def counter (atom 1)) ;; initialize counter for monitoring progress how many downloaded




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



(defn setMethod "string, int, string -> string sets http call for given method" [method userIdOrTrackId]
  (cond
   (= method "followers") (str "http://api.soundcloud.com/users/" userIdOrTrackId "/" method "?client_id=")
   (= method "followings") (str "http://api.soundcloud.com/users/" userIdOrTrackId "/" method "?client_id=")
   (= method "userInfo")  (str "http://api.soundcloud.com/users/" userIdOrTrackId "?client_id=")
   (= method "reposters") (str "https://api.soundcloud.com/e1/tracks/" userIdOrTrackId "/reposters?app_version=2d2d934&client_id=")
   (= method "likers") (str "http://api-v2.soundcloud.com/tracks/" userIdOrTrackId "231898794/likers?client_id=")
   (= method "")  (str "http://api.soundcloud.com/users/" userIdOrTrackId "?client_id=")
                                                                             ))


;; helper function checking if error 404 occured

(defn got404? [userid method offset] (= (try (client/get (str "http://api.soundcloud.com/users/" userid "/" method "?client_id=" soundcloud_client_id "&limit=100" "&linked_partitioning=1&offset=" offset) {:socket-timeout 2000 :conn-timeout 2000})
  (catch Exception e (str (.getMessage e)))) "clj-http: status 404"))


;; fixed function catching 404_errors

(defn sCCall "http call with choosen method and with client id and linked partitioning set to 100" [userid method offset]
  (let [methodString (setMethod method userid)
        fullString (str methodString soundcloud_client_id "&limit=100" "&linked_partitioning=1&offset=" offset)
;;         _ (println fullString)
        call100 (if (= method "likers") (try (httpCall (str "http://api-v2.soundcloud.com/tracks/" userid "/likers?client_id=" soundcloud_client_id "&limit=100")) (catch Exception e))
        (try (httpCall (str methodString  soundcloud_client_id "&limit=100" "&linked_partitioning=1&offset=" offset)) (catch Exception e)))]
    (if (= (:status call100) 200)
      call100
      (if (got404? userid method offset)
        (do (println (str "user doesn't exist!!!" userid)) (spit error_404_list (str userid "\n") :append true))
        (loop [retry 1]
          (if (< retry 10)
            (let [retry_call (try (httpCall (str methodString soundcloud_client_id "&limit=100" "&linked_partitioning=1&offset=" offset))
                               (catch Exception e))
                _ (println (str userid "_" offset "   will retry, retry: " retry))]
              (if (= (:status retry_call) 200)
                retry_call
                (recur (inc retry)))
              )))))))



(defn savesUserInfo "helper function used by get_all_users_requested to save user info" [userInfo]
  (let [userId (get userInfo "id")
        filename (str usersInfoPath userId ".edn")]
    (spit filename (with-out-str (pr userInfo)))))



(defn get_user_id "returns user id from user name" [user_name]
  "takes name of user and returns user id"
  (let [call (httpCall (str "http://api.soundcloud.com/users/" user_name "?client_id=" soundcloud_client_id))
        parsed_call (parse-string (:body call))]
    (get parsed_call "id")))










(defn get_userName "returns user name from user id" [userid]
  (let [call_rough (sCCall userid "userInfo" "")
        call (call_rough :body)
        parsed (parse-string call)]
    (get parsed "username")))



(defn get_userURL "returns user URL from user id" [userid]
  (let [call_rough (sCCall userid "userInfo" "")
        call (call_rough :body)
        parsed (parse-string call)]
    (get parsed "permalink_url")))




(defn get_userInfo [userIdorName]
  (let [userid (if (string? userIdorName) (get_user_id userIdorName) userIdorName)]
    (parse-string ((sCCall userid "userInfo" "") :body))))



(defn user_map "function converts string ids to key ids" [user]
  {:id (user "id"), :username (user "username"),
   :followers_count (user "followers_count"), :country (user "country"), :full_name (user "full_name"),
   :track_count (user "track_count") :plan (user "plan"), :followings_count (user "followings_count"), :last_modified (user "last_modified"), :description (user "description")
   })



(defn delete_partial_files "used by get_all_users_requested to delete partial files after successdully joining main file" [userid path]
  (loop [offset 0]
    (let [filename (str path userid "_" offset)]
      (if (try (io/delete-file filename) (catch Exception e))
        (recur (+ offset off))
        "deleted"))))



(defn get_requested_method "helper funcion used by get_all_users_requested" [userid offset method]
  (let [call_rough (sCCall userid method offset)]
        (if (= call_rough nil)      ; this means there was http error or timeout
          (do (swap! error_counter inc) (println (str "\n !!!!!            " method "    " userid "     ERROR   !!!!") )"error")
            (let [call (call_rough :body)
                parsed ((parse-string call) "collection")
                filename (str path "full" method "/" userid "_" offset)]
            (if (= call "{\"collection\":[]}")    ; this means end of collection
              "no_more_followers"
              (spit filename (with-out-str (pr parsed))))))))




(defn join_requested "used by get_all_users_requested to join sucessfully downloaded partial files" [userid method]
  (loop [offset 0, acc []]
    (let [filename (str path "full" method "/" userid "_" offset)
          try_requested (try (slurp filename)
                          (catch Exception e))]
      (if (= try_requested nil) acc
        (recur (+ offset off) (into acc (read-string try_requested)))))))



(defn get_all_users_requested "takes user id, method ('followings' or 'followers), downloads partial files (each conatining 100 followers or followings) after successdully getting all parts,
  deletes partial files, and save followings or followers file, userinfo file (containing info of user id),
  adds this user id to already_downloaded_file"[userid method]
  (let [path (str path "full" method "/")
        already_downloaded_file (str path "already_downloaded.edn")
        already_downloaded (edn/read-string (slurp already_downloaded_file))
        ]
    (if (contains? already_downloaded userid)
      (println (str userid " is already downloaded"))
      (loop [offset 0]
            (let [requested (get_requested_method userid offset method)]
              (if (= requested "error") (delete_partial_files userid path) ; encountered error delete partial files
                (if (= requested "no_more_followers") ; no more followers time to join them and log them
                  (let [userInfo (parse-string  ((sCCall userid "userInfo" 0) :body))]
                  (do (spit (str path userid ".edn") (with-out-str (pr (join_requested userid method))))
            (println "downloaded " (str @counter) " left " (str @howmanyleft))
            (delete_partial_files userid path)
;;             (println (str  "will save user info " userInfo))
            (savesUserInfo userInfo)
            (spit already_downloaded_file (with-out-str (pr (conj already_downloaded userid)))); adds sucessfully downloaded follower to logfile
            ))
        (recur (+ offset off))))))
      )))




;; sequential method to download all followers of user


(defn get_all_requested [userid method]
  (let [path (str path "full" method "/")
        logfile (str path "full" method "_log")
        already_downloaded (reduce #(conj %1 %2) (hash-set) (re-seq #"\w+" (slurp logfile))) ; reads from file and adds to hash-set
        users_dont_eixt (reduce #(conj %1 %2) (hash-set) (re-seq #"\w+" (slurp error_404_list))) ; reads users that don't exist anymore
        users_to_filter_out (into already_downloaded users_dont_eixt)
        requested_list (mapv (fn [x] ((user_map x) :id)) (read-string (slurp (str path userid ".edn")))) ; reads requested ids from file
        requested_not_yet_downloaded (filter #(not (contains? users_to_filter_out (str %))) requested_list) ; filters followers that are already downloaded or don't exit
;;         _ (println "sequential how many followers: " (count requested_list)
;;                    "difference: " (- (count requested_list) (count requested_not_yet_downloaded))
;;                    "sequential how many to download: " (count requested_not_yet_downloaded))
        ]
    (doall (map get_all_users_requested requested_not_yet_downloaded))))




;; chunked parallel method works faster than simple parallel method but leaves some followers not downloaded


(defn get_all_requested_chunked [userid method]
  (let [path (str path "full" method "/")
        logfile (str path "full" method "_log")
        already_exhausted (reduce #(conj %1 %2) (hash-set) (read-string (slurp (str path "already_downloaded.edn"))))]
    (if (contains? already_exhausted userid) (println "\n \n Already done \n")
  (let [already_downloaded (reduce #(conj %1 %2) (hash-set) (re-seq #"\w+" (slurp logfile)))
        users_dont_eixt (reduce #(conj %1 %2) (hash-set) (re-seq #"\w+" (slurp error_404_list)))
        users_to_filter_out (into already_downloaded users_dont_eixt)
        requested_list (mapv (fn [x] ((user_map x) :id)) (read-string (slurp (str path userid ".edn"))))
        requested_not_yet_downloaded (filter #(not (contains? users_to_filter_out (str %))) requested_list)
        number_of_requested_to_download (count requested_not_yet_downloaded)
        _ (def howmanyleft (atom number_of_requested_to_download))
        _ (println "how many followers: " (count requested_list)
                   "difference: " (- (count requested_list) number_of_requested_to_download)
                   "how many to download: " number_of_requested_to_download)
        partition_size (cond
                        (> number_of_requested_to_download 2700) 110 ; various chunk sizes depending on how many followers to download
                        (> number_of_requested_to_download 1800) 80
                        (> number_of_requested_to_download 1000) 64
                        (> number_of_requested_to_download 700) 40
                        (> number_of_requested_to_download 320) 20
                        (> number_of_requested_to_download 160) 12
                        (> number_of_requested_to_download 80) 8
                        (> number_of_requested_to_download 30) 6
                        (> number_of_requested_to_download 20) 5
                        (> number_of_requested_to_download 10) 4
                        (> number_of_requested_to_download 3) 2
                        :else 1)]
    (if (= number_of_requested_to_download 0) (spit (str path "already_downloaded.edn") (with-out-str (pr (conj already_exhausted userid))))
      (doall (pmap (fn [y] (doall (pmap (fn [x] (get_all_users_requested x method)) y ))) (partition partition_size requested_not_yet_downloaded))))))))




;; (get_all_requested_chunked 166399497 "followings")



;; function using both sequential and parallel methods to download followers

(defn id_sucker [id method]
  (let
    [path (str path "full" method "/")
     already_exhausted (reduce #(conj %1 %2) (hash-set) (read-string (slurp (str path "already_downloaded.edn"))))] ;don't download already d/loaded users
      (if (contains? already_exhausted id) (println "\n \n Already done \n")
        (do
           (loop [i 0]
             (when (< i 15) ; trying 15 times parallel d/load method
               (do (time (get_all_requested_chunked id method))
             (recur (inc i)))))
       (get_all_requested_chunked method) ; trying 1 time to d/load rest of followers with sequential method
       (println "errors " (str @error_counter) "user id" (str id))))))



(defn -main [method]
  (do (println "enter user id or ids: ")
    (let [ids (map read-string (re-seq #"\w+" (read-line)))]
      (time (doall (map (fn [x] (id_sucker x method)) ids))))))



(defn downloadUsersFromDirectory "downloads and saves all extracted user ids followers or followings depending on method argument" [directory method]
  (let [directoryName (str "/Volumes/ssd/SC/" directory)
        ids (filter #(not= % nil) (map (fn [x] (savers/getIdsfiles x method)) (seq (.list (clojure.java.io/file directoryName)))))]
    (doall (pmap (fn [x] (get_all_users_requested x "followings")) ids))))



;; (get_all_users_requested 496408 "followings")


(defn get_requested_from_list [ids method]
  (time (doall
   (pmap (fn [x] (get_all_users_requested x method)) ids))))

(get_requested_from_list (range 200000 (+ 200000 1000)) "followings")

