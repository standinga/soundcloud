(ns sc.core
   (:require [clj-http.client :as client]
             [cheshire.core :refer :all]
             [clojure.string]
             [net.cgrand.enlive-html :as html]
             [clojure.java.io :as io]
             ))


;; !!!!!
;; to run those functions you need enter below sound_client_id::::
(def soundcloud_client_id "af3e5e31e2e63ddad94791906ebddaec")

(def method "followers")

;; below are paths to store downloaded files with followers
;; !!! copy to path file with followers


(def path "/Volumes/ssd/SC/") ; set this path to store files with followers and copy there file with followers 140517.edn

;; copy file already_used.edn to this path
(def path1 "/Users/michal/sc/resources/")

;; logfile stores ids of already downloaded followers with theirs followers
(def logfile (str path1 "SClog"))
(def datafile (str path1 "SCdatafile.txt"))

(def off 100) ; sets  pagination offset

(def error_404_list (str path1 "error404list"))


(def error_counter (atom 1)) ; initialize error counter to monitor how many d/loads were wrong

(def howmanyleft (atom 0))   ; initialize to zero how many left to download

(def counter (atom 1)) ;; initialize counter for monitoring progress how many downloaded


(defn httpCall [url]
  (client/get url {:socket-timeout 2000 :conn-timeout 2000}))

;; reposters
(:body (httpCall "https://api.soundcloud.com/e1/tracks/222501662/reposters?app_version=2d2d934&client_id=02gUJC0hH2ct1EGOcYXQIzRFU91c72Ea&limit=25&linked_partitioning=1&offset=00"))
;;likers
(httpCall "https://api-v2.soundcloud.com/tracks/222501662/likers?limit=100&offset=0&linked_partitioning=1&client_id=02gUJC0hH2ct1EGOcYXQIzRFU91c72Ea&app_version=2d2d934")
(httpCall "http://api-v2.soundcloud.com/tracks/231898794/likers?client_id=af3e5e31e2e63ddad94791906ebddaec")
;; (httpCall "http://sh191485.website.pl/joe/")

;; notifications

;;likes
(httpCall "https://api-v2.soundcloud.com/users/107508374/likes?limit=10&offset=0&linked_partitioning=1&client_id=02gUJC0hH2ct1EGOcYXQIzRFU91c72Ea&app_version=07a58eb")






(get (:content (get (:content (get (:content (get (:content (get (:content (get (:content (get (:content (get (:content (client/parse-html  "http://meteo.waw.pl/")) 1)) 15)) 13)) 3)) 1)) 1)) 1)) 0)


;; helper function checking if error 404 occured

(defn get_user_id [user_name]
  "takes name of user and returns user id"
  (let [call (httpCall (str "http://api.soundcloud.com/users/" user_name "?client_id=" soundcloud_client_id))
        parsed_call (parse-string (:body call))]
    (get parsed_call "id")))


(defn got404? [userid method offset] (= (try (client/get (str "http://api.soundcloud.com/users/" userid "/" method "?client_id=" soundcloud_client_id "&limit=100" "&linked_partitioning=1&offset=" offset) {:socket-timeout 2000 :conn-timeout 2000})
  (catch Exception e (str (.getMessage e)))) "clj-http: status 404"))


;; fixed function catching 404_errors

(defn SCcall100 "http call with choosen method and with client id and linked partitioning set to 100" [userid method offset]
  (let [call100 (try (httpCall (str "http://api.soundcloud.com/users/" userid "/" method "?client_id=" soundcloud_client_id "&limit=100" "&linked_partitioning=1&offset=" offset))
        (catch Exception e))]
    (if (= (:status call100) 200)
      call100
      (if (got404? userid method offset)
        (do (println (str "user doesn't exist!!!" userid)) (spit error_404_list (str userid "\n") :append true))
        (loop [retry 1]
          (if (< retry 10)
            (let [retry_call (try (httpCall (str "http://api.soundcloud.com/users/" userid "/" method "?client_id=" soundcloud_client_id "&limit=100" "&linked_partitioning=1&offset=" offset))
                               (catch Exception e))
                _ (println (str userid "_" offset "   will retry, retry: " retry))]
              (if (= (:status retry_call) 200)
                retry_call
                (recur (inc retry)))
              )))))))


(defn user_map [user]
  {:id (user "id"), :username (user "username"),
   :followers_count (user "followers_count"), :country (user "country"), :full_name (user "full_name"),
   :track_count (user "track_count") :plan (user "plan"), :followings_count (user "followings_count"), :last_modified (user "last_modified"), :description (user "description")
   })


(defn delete_partial_files "used to deleted partially downloaded files when all of partial files are succesflly downloaded" [userid]
  (loop [offset 0]
    (let [filename (str path userid "_" offset)]
      (if (try (io/delete-file filename) (catch Exception e))
        (recur (+ offset off))
        "deleted"))))


(defn get_userName [userid]
  (let [call_rough (SCcall100 userid "" "")
        call (call_rough :body)
        parsed (parse-string call)]
    (get parsed "username")))

(get_userName 772854)



(defn get_followers [userid offset]
  (let [call_rough (SCcall100 userid "followers" offset)]
        (if (= call_rough nil)      ; this means there was http error or timeout
          (do (swap! error_counter inc) (println (str "\n !!!!!            "userid "     ERROR   !!!!") )"error")
;;           (do (spit (str path "error") (str userid offset "\n") :append true) (swap! error_counter inc) "error")
            (let [call (call_rough :body)
                parsed ((parse-string call) "collection")
                filename (str path userid "_" offset)

;; below commented out are extra features showing progress of downloading parts

;;                 _ (print (str "*"
;;                               userid "_" offset
;;                               ))
                  ]
            (if (= call "{\"collection\":[]}")    ; this means end of collection
              "no_more_followers"
              (spit filename (with-out-str (pr parsed))))))))


(defn join_followers "used to join sucessfully downloaded parts" [userid]
  (loop [offset 0, acc []]
    (let [filename (str path userid "_" offset)
          try_followers (try (slurp filename)
                          (catch Exception e))]
      (if (= try_followers nil) acc
        (recur (+ offset off) (into acc (read-string try_followers)))))))


(defn get_all_users_followers [userid]
  (loop [offset 0]
    (let [followers (get_followers userid offset)]
      (if (= followers "error") (delete_partial_files userid) ; encountered error delete partial files
        (if (= followers "no_more_followers") ; no more followers time to join them and log them
          (do (spit (str path userid ".edn") (with-out-str (pr (join_followers userid))))
          (spit logfile (str userid "\n") :append true) ; adds sucessfully downloaded follower to logfile
          (println "downloaded " (str @counter) " left " (str @howmanyleft)
;;                    (str userid) ; extra feature showing which follower was downloaded
                   )
          (swap! howmanyleft dec)
;;           (println " ")
          (swap! counter inc)
          (delete_partial_files userid))
        (recur (+ offset off)))))))


;; sequential method to download all followers of user

(defn get_all_followers_of_users_followers [userid]
  (let [already_downloaded (reduce #(conj %1 %2) (hash-set) (re-seq #"\w+" (slurp logfile))) ; reads from file and adds to hash-set
        users_dont_eixt (reduce #(conj %1 %2) (hash-set) (re-seq #"\w+" (slurp error_404_list))) ; reads users that don't exist anymore
        users_to_filter_out (into already_downloaded users_dont_eixt)
        followers_list (mapv (fn [x] ((user_map x) :id)) (read-string (slurp (str path userid ".edn")))) ; reads followers from file
        followers_not_yet_downloaded (filter #(not (contains? users_to_filter_out (str %))) followers_list) ; filters followers that are already downloaded or don't exit
        _ (println "sequential how many followers: " (count followers_list)
                   "difference: " (- (count followers_list) (count followers_not_yet_downloaded))
                   "sequential how many to download: " (count followers_not_yet_downloaded))]
    (doall (map get_all_users_followers followers_not_yet_downloaded))))


;; chunked parallel method works faster than simple parallel method but leaves some followers not downloaded

(defn get_all_followers_of_users_followers_chunked [userid]
  (let [already_exhausted (reduce #(conj %1 %2) (hash-set) (read-string (slurp (str path1 "already_used.edn"))))]
    (if (contains? already_exhausted userid) (println "\n \n Already done \n")
  (let [already_downloaded (reduce #(conj %1 %2) (hash-set) (re-seq #"\w+" (slurp logfile)))
        users_dont_eixt (reduce #(conj %1 %2) (hash-set) (re-seq #"\w+" (slurp error_404_list)))
        users_to_filter_out (into already_downloaded users_dont_eixt)
        followers_list (mapv (fn [x] ((user_map x) :id)) (read-string (slurp (str path userid ".edn"))))
        followers_not_yet_downloaded (filter #(not (contains? users_to_filter_out (str %))) followers_list)
        number_of_followers_to_download (count followers_not_yet_downloaded)
        _ (def howmanyleft (atom number_of_followers_to_download))
        _ (println "how many followers: " (count followers_list)
                   "difference: " (- (count followers_list) number_of_followers_to_download)
                   "how many to download: " number_of_followers_to_download)
        partition_size (cond
                        (> number_of_followers_to_download 2700) 110 ; various chunk sizes depending on how many followers to download
                        (> number_of_followers_to_download 1800) 80
                        (> number_of_followers_to_download 1000) 64
                        (> number_of_followers_to_download 700) 40
                        (> number_of_followers_to_download 320) 20
                        (> number_of_followers_to_download 160) 12
                        (> number_of_followers_to_download 80) 8
                        (> number_of_followers_to_download 30) 6
                        (> number_of_followers_to_download 20) 5
                        (> number_of_followers_to_download 10) 4
                        (> number_of_followers_to_download 3) 2
                        :else 1)]
    (if (= number_of_followers_to_download 0) (spit (str path1 "already_used.edn") (with-out-str (pr (conj already_exhausted userid))))
      (doall (pmap (fn [x] (doall (pmap get_all_users_followers x))) (partition partition_size followers_not_yet_downloaded))))))))

;; function using both sequential and parallel methods to download followers

(defn id_sucker [id]
  (let
    [already_exhausted (reduce #(conj %1 %2) (hash-set) (read-string (slurp (str path1 "already_used.edn"))))] ;don't download already d/loaded users
      (if (contains? already_exhausted id) (println "\n \n Already done \n")
        (do
           (loop [i 0]
             (when (< i 15) ; trying 15 times parallel d/load method
               (do (time (get_all_followers_of_users_followers_chunked id))
             (recur (inc i)))))
       (get_all_followers_of_users_followers id) ; trying 1 time to d/load rest of followers with sequential method
       (println "errors " (str @error_counter) "user id" (str id))))))



(defn -main []
  (do (println "enter user id or ids: ")
    (let [ids (map read-string (re-seq #"\w+" (read-line)))]
      (time (doall (map id_sucker ids))))))


;; (get_all_users_followers (get_all_users_followers 181992632))

;; (get_all_users_followers 331212)

; main method when calling from terminal enter 140517 , later you can call with multiple uers ids (they need to be already downloaded)

;; (-main)

(defn count_followers_aux [userid]
  (let [call (httpCall (str "http://api.soundcloud.com/users/" userid "?client_id=" soundcloud_client_id))
        parsed_call (parse-string (:body call))
        username (get parsed_call "username")
        followers_count (mapv (fn [x] ((user_map x) :followers_count)) (read-string (slurp (str path userid ".edn"))))
        biggest_followers_list (mapv (fn [x] [((user_map x) :followers_count) ((user_map x) :id)]) (read-string (slurp (str path userid ".edn"))))
        biggest_followers (sort-by first biggest_followers_list)
        top20_followers (take 20 (reverse biggest_followers))
        top20_followers_names (map (fn [x] [(x 0) (x 1) (get_userName (x 1))]) top20_followers)
;;         _ (println top20_followers)
        _ (println top20_followers_names)
        followers_sum (reduce + followers_count)
        following_count (mapv (fn [x] ((user_map x) :followings_count)) (read-string (slurp (str path userid ".edn"))))
        following_sum (reduce + following_count)
        biggest_followings_list  (mapv (fn [x] [((user_map x) :followings_count) ((user_map x) :id)]) (read-string (slurp (str path userid ".edn"))))
        biggest_followings (sort-by first biggest_followings_list)
        top20_followings (take 20 (reverse biggest_followings))
        top20_followings_names (map (fn [x] [(x 0) (x 1) (get_userName (x 1))]) top20_followings)

        difference  (- followers_sum following_sum)
        ratio (double  (/ followers_sum following_sum))
        rating (int (* followers_sum ratio))
        text (str "\n" username "\t\t" " followers: " followers_sum " follow: " following_sum " difference: " difference " ratio: " (format "%.2f" ratio) " rating: " rating)
        _ (println text)
        text1 (str "\n Biggest followings of " username "\t\t" " ")
;;         _ (println text1)
        _ (println top20_followings_names)]


    (spit datafile text :append true)
    ))

(.exists (io/as-file (str path 51463492 ".edn")))

;; (get_user_id "dj_bojan_remixes")

(defn count_followers [user]
  (let [userid (if (string? user) (get_user_id user) user)]
    (if (.exists (io/as-file (str path userid ".edn"))) (count_followers_aux userid)
      (do  (get_all_users_followers userid) (count_followers_aux userid)))))

(count_followers "jackin-deep-future-house")

;; (count_followers 51463492)

;; (mapv (fn [x] ((user_map x) :followers_count)) (read-string (slurp (str path 181992632 ".edn"))))
