(ns sc.analyze
   (:require [clj-http.client :as client]
             [cheshire.core :refer :all]
             [clojure.string]
             [net.cgrand.enlive-html :as html]
             [clojure.java.io :as io]
             [clojure.edn :as edn]
             [sc.inits :refer :all]
             [sc.getters :as getters]
             ))


;; !!!!!
;; to run those functions you need enter below sound_client_id::::

(def error_counter (atom 1)) ; initialize error counter to monitor how many d/loads were wrong

(def howmanyleft (atom 0))   ; initialize to zero how many left to download

(def counter (atom 1)) ;; initialize counter for monitoring progress how many downloaded





(defn count_followers_aux [userid method]
  (let [call (getters/httpCall (str "http://api.soundcloud.com/users/" userid "?client_id=" soundcloud_client_id))
        parsed_call (parse-string (:body call))
        username (get parsed_call "username")
        followers_count (mapv (fn [x] ((getters/user_map x) :followers_count)) (read-string (slurp (str path userid ".edn"))))
        method_followers_list (mapv (fn [x] [((getters/user_map x) :followers_count) ((getters/user_map x) :id)]) (read-string (slurp (str path userid ".edn"))))
        biggest_followers  (sort-by first method_followers_list)
        top20_followers (take 20 (reverse biggest_followers))
        top20_followers_names (map (fn [x] [(x 0) (x 1) (getters/get_userName (x 1))]) top20_followers)
        top20_smallest_followers (take 20  biggest_followers)
        top20_smallest_followers_names (map (fn [x] [(x 0) (x 1) (getters/get_userName (x 1))]) top20_smallest_followers)

        _ (if (= method ">") (println "biggest followers: " top20_followers_names) (println "smallest followers: " top20_smallest_followers_names))

        followers_sum (reduce + followers_count)
        following_count (mapv (fn [x] ((getters/user_map x) :followings_count)) (read-string (slurp (str path userid ".edn"))))
        following_sum (reduce + following_count)

        method_followings_list  (mapv (fn [x] [((getters/user_map x) :followings_count) ((getters/user_map x) :id) ((getters/user_map x) :followers_count)]) (read-string (slurp (str path userid ".edn"))))
        method_followings (sort-by first method_followings_list)

        top20_followings (take 20 (reverse method_followings))
        top20_followings_names (map (fn [x] [(x 0) (x 1) (getters/get_userName (x 1))]) top20_followings)
        top20_smallest_followings (take 20 method_followings)
         top20_smallest_followings_names (map (fn [x] [(x 0) (x 1) (getters/get_userName (x 1))]) top20_smallest_followings)
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

