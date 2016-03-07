(ns sc.savers
   (:require [clj-http.client :as client]
             [cheshire.core :refer :all]
             [clojure.string]
             [net.cgrand.enlive-html :as html]
             [clojure.java.io :as io]
             [clojure.edn :as edn]
             [sc.inits :refer :all]
             [sc.getters :as getters]
             ))



(defn addUserIdtoFile [userMethod userId method]
  (let [fileTo (str "/Volumes/ssd/SC/" method "/" userId ".edn")
        userIdsAlreadyThere (if (.exists (io/as-file fileTo)) (edn/read-string (slurp fileTo)) (hash-set))
        usersToAdd (conj userIdsAlreadyThere (read-string userMethod))
;;         _ (println (str "addUserIdtoFile" usersToAdd userMethod userId method fileTo))

        ]
;;     (println (str "add user id to file" userMethod userId))
    (spit fileTo (with-out-str (pr usersToAdd)))))



(defn methodToMethod  "helper function used by extractMethod, takes user id, list of user ids method from which user ids were and method to which it will convert ids "[userId usersIds methodFrom methodTo]
  (if (= methodFrom methodTo)
      (let [fileTo (str "/Volumes/ssd/SC/" methodTo "/" userId ".edn")
;;             _ (println (str "methodToMethod " methodFrom "   " methodTo))
          userIdsAlreadyThere (if (.exists (io/as-file fileTo)) (edn/read-string (slurp fileTo)) (hash-set))
          userToAdd (into userIdsAlreadyThere usersIds)]
          (spit fileTo (with-out-str (pr userToAdd))))
    (doall (map (fn [x] (addUserIdtoFile userId x methodTo)) usersIds))))



(defn extractMethod "extracts followers of followings from given userId, method: 'followers' or 'followings' " [userId method]
  (if (= userId nil) "used"
  (let [alreadyDoneFileName (str "/Volumes/ssd/SC/full" method "/already_used.edn")
        alreadyDone (edn/read-string (slurp alreadyDoneFileName))
;;         _ (println (str "ExtactMethod     userIdOrName  "  userId  ))

        methodTo (if (= method "followers") "followings" "followers")
        extracted (edn/read-string (slurp (str "/Volumes/ssd/SC/full" method "/"userId ".edn")))
        extractedIds (map (fn [x] ((getters/user_map x) :id)) extracted)
;;         _ (println (str "userID " userId " extracted " method ": " (count extractedIds)))
        ]
     (do   (methodToMethod userId extractedIds method methodTo)
     (methodToMethod userId extractedIds method method)

       )

  (spit alreadyDoneFileName (with-out-str (pr (conj alreadyDone userId)))
;;        (println (str " already done "  userId))
      )
    )))




(defn getIdsfiles "helper function used by getAllIdsFromMethod kates filename and ('followers' or 'followings' methods)
  used to filter out already downloaded files, files not containing user ids" [fileName method]
  (let [alreadyDone (edn/read-string (slurp (str "/Volumes/ssd/SC/full" method "/already_used.edn")))
        fileNoExtension (subs fileName 0 (- (count fileName) 4))]
  (if (and
       (not= fileName "deleted.edn")
       (not= fileName "already_used.edn")
       (not= fileName "already_downloaded.edn")
       (= (subs fileName (- (count fileName) 4)) ".edn")
       (not (contains? alreadyDone fileNoExtension))
       )
    fileNoExtension)))


(defn getAllIdsFromMethod "takes 'method' ('followers' or 'followings') resolves it to directory containing files with ids returns sequence of users ids" [method]
  (let [directoryName (str "/Volumes/ssd/SC/full" method)]
    (map  (fn [x] (getIdsfiles x method)) (seq (.list (clojure.java.io/file directoryName))))))


(defn extractAllMethod "tales method 'followers' or 'followings' and returns seq of ids" [method]
  (let [userIds (getAllIdsFromMethod method)
        notYetUsedIds (filter #(not= % nil) userIds)

        _ (println (str "number of ids to extract: " (count notYetUsedIds)))
        ]
    (doall (map (fn [x] (extractMethod x method)) userIds))))

;; (time (extractAllMethod "followings"))



;; (extractAllMethod "followers")

;; (contains? alreadyDone 49435864)

;; (getAllIdsFromMethod "followers")

;; (getters/get_all_users_requested 150867001 "followings")


;; (downloadUsersFromDirectory "1" "followings")
;; (getters/get_all_users_requested 184875783 "followings")
;; (getters/get_userName 150867001)

;; (def directoryName (str "/Volumes/ssd/SC/" "1"))
;; (seq (.list (clojure.java.io/file directoryName)))
;; (filter #(not= % nil) (map (fn [x] (getIdsfiles x "followings")) (seq (.list (clojure.java.io/file directoryName)))))

(defn howManyFollowersDownloaded "reads how many ids or users in filename" [userId]
  (let [root "/Volumes/ssd/SC/followers/"
        fileName (str root userId ".edn")
        mapOfIds (edn/read-string (slurp fileName))
        followersDownloaded (count mapOfIds)
        fullFollowersNumber ((getters/get_userInfo userId) "followers_count")
        userName ((getters/get_userInfo userId) "username")
        progress (format "%.2f" (* 100 (float (/ followersDownloaded fullFollowersNumber))))
        ]
    (println (str "user " userId " name " userName " has " followersDownloaded " followers downloaded out of total " fullFollowersNumber ", progress so far: " progress "%"))))


(defn howManyFollowersDownloadedBig "reads how many ids or users in filename" [userId]
  (let [followersBig (edn/read-string (slurp "/Volumes/ssd/SC/followersBig.edn"))
        mapOfIds (get followersBig userId)
        followersDownloaded (count mapOfIds)
        fullFollowersNumber ((getters/get_userInfo userId) "followers_count")
        userName ((getters/get_userInfo userId) "username")
        progress (format "%.2f" (* 100 (float (/ followersDownloaded fullFollowersNumber))))
        ]
    (println (str "user " userId " name " userName " has " followersDownloaded " followers downloaded out of total " fullFollowersNumber ", progress so far: " progress "%"))))



(howManyFollowersDownloaded 324471)
(howManyFollowersDownloadedBig 324471)
;; (howManyFollowersDownloaded 198443)
;; (howManyFollowersDownloaded 26595)
;; (howManyFollowersDownloaded 107508374)
;; (howManyFollowersDownloaded 99914158)
;; (howManyFollowersDownloaded 215)
;; (howManyFollowersDownloaded 57357)


;;  (get_all_requested 143069245 "followers")

(defn getIntegerFromInput "parse into Integer, if already integer returns input" [input]
  (if  (integer? input) input
            (let [y (try (read-string input) (catch Exception e nil))]
              (if (integer? y) y))))



(defn addToFollowersBig "adds ids from file to FollowersBig" [id followersBig]
  (let [fileAddress (str "/Volumes/ssd/SC/followers/" id ".edn")
        followers (if (.exists (io/as-file fileAddress)) (edn/read-string (slurp fileAddress)) (hash-set))
        followersParsed (into #{} (map getIntegerFromInput followers))
;;         _ (println (str followers "  "  followersParsed))
        ]
    (assoc followersBig id followersParsed)))


(defn createsFollowersBigAux "creates new FolloersBig hash-map from list of files containing followers"  [listOfFiles followersBig]
  (if (empty? listOfFiles) followersBig
    (let [
;;           _ (println (first listOfFiles))
          newfollowersBig (addToFollowersBig (first listOfFiles) followersBig)]
      (recur (rest listOfFiles) newfollowersBig))))


(defn removesExtension [fileName]
  (if (> (count fileName) 4)
    (subs fileName 0 (- (count fileName) 4))))




(defn createsFollowersBig []
  (let [directoryName "/Volumes/ssd/SC/followers"
        fileTo (str "/Volumes/ssd/SC/" "followers" "Big"  ".edn")
        listOfFiles  (seq (.list (clojure.java.io/file directoryName)))
        listOfFilesNoExtension (map removesExtension listOfFiles)
        listOfParsed (map getIntegerFromInput listOfFilesNoExtension)
        listOfIds (filter #(not= % nil) listOfParsed)
        followersBig (time (createsFollowersBigAux listOfIds (hash-map)))
        ]
      (time (spit fileTo (with-out-str (pr followersBig))))))


(def directoryName "/Volumes/ssd/SC/followers")

(def filenames (seq (.list (clojure.java.io/file directoryName))))


(def filesnoext (map removesExtension filenames))
(def listOfParsed (map (fn [x] (let [y (try (read-string x) (catch Exception e nil))] (if (integer? y) y))) filesnoext))
(def listOfIds (filter #(not= % nil) listOfParsed))
listOfIds


;; (map (fn [x] (let [y (try (read-string x) (catch Exception e nil))] (if (integer? y) y))) (seq (.list (clojure.java.io/file directoryName))))

;; (createsFollowersBigAux (list 1 100) (hash-map))

;; (createsFollowersBig)



(def haszmap {2 #{1 3} 3 #{4} 4 #{1 2 3}})
haszmap


(defn updateHashMap [haszmap keymap value]
  (let [alreadyThere (get haszmap keymap)
        newValue (conj alreadyThere value)]
    (assoc haszmap keymap newValue)))

(updateHashMap haszmap 4 9)

(get haszmap 4)

;; (def a (addToFollowersBig 2075842 {}))

a




;; (time (createsFollowersBig))

(def textset #{"1" "2" "3"})

textset

(into #{} (map read-string textset))
