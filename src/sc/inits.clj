(ns sc.inits)

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

(def directory (clojure.java.io/file path))

(def files (file-seq directory))
