(ns sc.getters
   (:require [clj-http.client :as client]
             [cheshire.core :refer :all]
             [clojure.string]
             [net.cgrand.enlive-html :as html]
             [clojure.java.io :as io]
             [clojure.edn :as edn]
             ))


;; !!!!!
;; to run those functions you need enter below sound_client_id::::
(def soundcloud_client_id "af3e5e31e2e63ddad94791906ebddaec")

;; (def method "followers")

;; below are paths to store downloaded files with followers
;; !!! copy to path file with followers


(def path "/Volumes/ssd/SC/") ; set this path to store files with followers and copy there file with followers 140517.edn

(def usersInfoPath "/Volumes/ssd/SC/usersInfo/")

(def followersDir "/Volumes/ssd/SC/followers/")

(def followingsDir "/Volumes/ssd/SC/followings/")

(def followingsFullDir "/Volumes/ssd/SC/fullfollowings/")

(def followersFullDir "/Volumes/ssd/SC/fullfollowers/")

;; copy file already_used.edn to this path
(def path1 "/Users/michal/sc/resources/")

;; logfile stores ids of already downloaded followers with theirs followers


(def logfile (str path1 "SClog"))

(def followings_logfile (str followingsFullDir "followings_log"))

(def datafile (str path1 "SCdatafile.txt"))

(def off 100) ; sets  pagination offset

(def error_404_list (str path1 "error404list"))


(def error_counter (atom 1)) ; initialize error counter to monitor how many d/loads were wrong

(def howmanyleft (atom 0))   ; initialize to zero how many left to download

(def counter (atom 1)) ;; initialize counter for monitoring progress how many downloaded


(def directory (clojure.java.io/file path))
(def files (file-seq directory))


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


;; (get_all_users_requested 170182504 "followers")


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




; main method when calling from terminal enter 140517 , later you can call with multiple uers ids (they need to be already downloaded)

;; (-main)

(defn count_followers_aux [userid method]
  (let [call (httpCall (str "http://api.soundcloud.com/users/" userid "?client_id=" soundcloud_client_id))
        parsed_call (parse-string (:body call))
        username (get parsed_call "username")
        followers_count (mapv (fn [x] ((user_map x) :followers_count)) (read-string (slurp (str path userid ".edn"))))
        method_followers_list (mapv (fn [x] [((user_map x) :followers_count) ((user_map x) :id)]) (read-string (slurp (str path userid ".edn"))))
        biggest_followers  (sort-by first method_followers_list)
        top20_followers (take 20 (reverse biggest_followers))
        top20_followers_names (map (fn [x] [(x 0) (x 1) (get_userName (x 1))]) top20_followers)
        top20_smallest_followers (take 20  biggest_followers)
        top20_smallest_followers_names (map (fn [x] [(x 0) (x 1) (get_userName (x 1))]) top20_smallest_followers)

        _ (if (= method ">") (println "biggest followers: " top20_followers_names) (println "smallest followers: " top20_smallest_followers_names))

        followers_sum (reduce + followers_count)
        following_count (mapv (fn [x] ((user_map x) :followings_count)) (read-string (slurp (str path userid ".edn"))))
        following_sum (reduce + following_count)

        method_followings_list  (mapv (fn [x] [((user_map x) :followings_count) ((user_map x) :id) ((user_map x) :followers_count)]) (read-string (slurp (str path userid ".edn"))))
        method_followings (sort-by first method_followings_list)

        top20_followings (take 20 (reverse method_followings))
        top20_followings_names (map (fn [x] [(x 0) (x 1) (get_userName (x 1))]) top20_followings)
        top20_smallest_followings (take 20 method_followings)
         top20_smallest_followings_names (map (fn [x] [(x 0) (x 1) (get_userName (x 1))]) top20_smallest_followings)
        difference  (- followers_sum following_sum)
        ratio (double  (/ followers_sum following_sum))
        rating (int (* followers_sum ratio))
        text (str "\n" username "\t\t" " followers: " followers_sum " follow: " following_sum " difference: " difference " ratio: " (format "%.2f" ratio) " rating: " rating)
        _ (println text)
;;         text1 (str "\n Biggest followings of " username "\t\t" " ")
;;         _ (println text1)
;;         _ (println top20_followings_names)
        _ (if (= method ">") (println "biggest followints: " top20_followings_names) (println "smallest followints: " top20_smallest_followings_names))

        ]


    (spit datafile text :append true)
    ))


;; (get_user_id "dj_bojan_remixes")

;; (defn count_followers [user method]
;;   (let [userid (if (string? user) (get_user_id user) user)]
;;     (if (.exists (io/as-file (str path userid ".edn"))) (count_followers_aux userid method)
;;       (do  (get_all_users_followers userid) (count_followers_aux userid method)))))

;; (get_all_users_followers (get_user_id "sutasin-srijunsawaung"))

;; map->void takes map and writes it into separate .edn file containing user info




(defn addUserIdtoFile [userMethod userId method]
  (let [fileTo (str "/Volumes/ssd/SC/" method "/" userId ".edn")
        userIdsAlreadyThere (if (.exists (io/as-file fileTo)) (edn/read-string (slurp fileTo)) (hash-set))

        usersToAdd (conj userIdsAlreadyThere userMethod)]
    (spit fileTo (with-out-str (pr usersToAdd)))))



(defn methodToMethod  "helper function used by extractMethod, takes user id, list of user ids method from which user ids were and method to which it will convert ids "[userId usersIds methodFrom methodTo]
    (if (= methodFrom methodTo)
      (let [fileTo (str "/Volumes/ssd/SC/" methodTo "/" userId ".edn")
;;             _ (println fileTo)
          userIdsAlreadyThere (if (.exists (io/as-file fileTo)) (edn/read-string (slurp fileTo)) (hash-set))
          userToAdd (into userIdsAlreadyThere usersIds)]
          (spit fileTo (with-out-str (pr userToAdd))))
      (doall (map (fn [x] (addUserIdtoFile userId x methodTo)) usersIds))))



(defn extractMethod "extracts followers of followings from given userId, method: 'followers' or 'followings' " [userIdOrName method]
  (if (not= userIdOrName nil)
  (let [alreadyDoneFileName (str "/Volumes/ssd/SC/full" method "/already_used.edn")
        alreadyDone (edn/read-string (slurp alreadyDoneFileName))
        userId (if (string? userIdOrName) (get_user_id userIdOrName) userIdOrName)
        methodTo (if (= method "followers") "followings" "followers")
        extracted (edn/read-string (slurp (str "/Volumes/ssd/SC/full" method "/"userId ".edn")))
        extractedIds (map (fn [x] ((user_map x) :id)) extracted)
        _ (println (str "userID " userId " extracted " method ": " (count extractedIds)))]
    (methodToMethod userId extractedIds method method)
    (methodToMethod userId extractedIds method methodTo)
    (spit alreadyDoneFileName (with-out-str (pr (conj alreadyDone userId))))
    )))

;; (extractMethod 4088604 "followings")


(defn getIdsfiles "helper function used by getAllIdsFromMethod kates filename and ('followers' or 'followings' methods)
  used to filter out already downloaded files, files not containing user ids" [fileName method]
  (let [alreadyDone (edn/read-string (slurp (str "/Volumes/ssd/SC/full" method "/already_used.edn")))
        fileNoExtension (subs fileName 0 (- (count fileName) 4))]
  (if (and
       (not= fileName "deleted.edn")
       (not= fileName "already_used.edn")
       (not= fileName "already_downloaded.edn")
       (= (subs fileName (- (count fileName) 4)) ".edn")
       (not (contains? alreadyDone (read-string fileNoExtension)))
       )
    fileNoExtension)))


(defn getAllIdsFromMethod "takes 'method' ('followers' or 'followings') resolves it to directory containing files with ids returns sequence of users ids" [method]
  (let [directoryName (str "/Volumes/ssd/SC/full" method)]
    (map  (fn [x] (getIdsfiles x method)) (seq (.list (clojure.java.io/file directoryName))))))


(defn extractAllMethod "tales method 'followers' or 'followings' and returns seq of ids" [method]
  (let [userIds (getAllIdsFromMethod method)
        _ (println userIds)
        ]
    (doall (pmap (fn [x] (extractMethod x method)) userIds))))

;; (extractAllMethod "followings")

;; (contains? alreadyDone 49435864)

;; (getAllIdsFromMethod "followers")

;; (get_all_users_requested 150867001 "followings")

(defn downloadUsersFromDirectory "downloads and saves all extracted user ids followers or followings depending on method argument" [directory method]
  (let [directoryName (str "/Volumes/ssd/SC/" directory)
        ids (filter #(not= % nil) (map (fn [x] (getIdsfiles x method)) (seq (.list (clojure.java.io/file directoryName)))))]
    (doall (pmap (fn [x] (get_all_users_requested x "followings")) ids))))

;; (downloadUsersFromDirectory "1" "followings")
;; (get_all_users_requested 184875783 "followings")
;; (get_userName 150867001)

;; (def directoryName (str "/Volumes/ssd/SC/" "1"))
;; (seq (.list (clojure.java.io/file directoryName)))
;; (filter #(not= % nil) (map (fn [x] (getIdsfiles x "followings")) (seq (.list (clojure.java.io/file directoryName)))))

(defn howManyFollowersDownloaded "reads how many ids or users in filename" [userId]
  (let [root "/Volumes/ssd/SC/followers/"
        fileName (str root userId ".edn")
        mapOfIds (edn/read-string (slurp fileName))
        followersDownloaded (count mapOfIds)
        fullFollowersNumber ((get_userInfo userId) "followers_count")
        userName ((get_userInfo userId) "username")
        progress (format "%.2f" (* 100 (float (/ followersDownloaded fullFollowersNumber))))
        ]
    (println (str "user " userId " name " userName " has " followersDownloaded " followers downloaded out of total " fullFollowersNumber ", progress so far: " progress "%"))))

;; (howManyFollowersDownloaded 324471)
;; (howManyFollowersDownloaded 198443)
;; (howManyFollowersDownloaded 26595)


;;  (get_all_requested 143069245 "followers")
;; (get_all_users_requested 143069245 "followers")
;; (get_user_id "jack-reid-47")

(extractAllMethod "followers")

;; (time (doall (pmap (fn [x] (get_all_users_requested x "followings"))
;;             [36858776 149825847 150862276 176483129 100069324 143269703 144501172 91903838 150158352 122910633 139540020 139422050 149524788 156935340 54526897])))

