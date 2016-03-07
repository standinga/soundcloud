(ns sc.core
   (:require [clj-http.client :as client]
             [clojure.data.json :as json]
             [crouton.html :as html]
             [cheshire.core :refer :all]
;;              [quil.core :as q]
;;              [quil.middleware :as m]
             [clojure.string]
             [clojure.java.io :as io]))


(def soundcloud_client_id "af3e5e31e2e63ddad94791906ebddaec")
(def soundcloud_secret_key "f492cc026e9902440e2198bacc2b7359")

(def ja "107508374")
(def method "followers")
(def maly "122447327")
(def wawa "1763025")
(def user ja)
(def path "/Users/michal/sc/resources/SC/")

(defn httpCall
  [url]
  (client/get url))


(def test1 (httpCall "http://api.soundcloud.com/users/3207?client_id=af3e5e31e2e63ddad94791906ebddaec"))
(def test2 (httpCall "http://api.soundcloud.com/users/1763025?client_id=af3e5e31e2e63ddad94791906ebddaec"))

(test2 :body)

(defn SCcall [userid method]
  (httpCall (str "http://api.soundcloud.com/users/" userid "/" method "?client_id=" soundcloud_client_id)))

(defn SCcall100 [userid method offset]
  (try
    (httpCall (str "http://api.soundcloud.com/users/" userid "/" method "?client_id=" soundcloud_client_id "&limit=100" "&linked_partitioning=1&offset=" offset))
    (catch Exception e)))

;; (SCcall100 "393793311" "" 0)


;; (SCcall "122447327" "followers" )

;; ((SCcall "122447327" "followers" ):body)

;; ((SCcall100 "107508374" "followers" 0):body)
;; ((SCcall100 "107508374" "followers" 3000):body)

;; (((parse-string ((SCcall100 "107508374" "followers" 2800):body)) "collection") 11)
;; (def parsedusers ((parse-string ((SCcall100 "107508374" "followers" 2800):body)) "collection"))


(defn user_map [user]
  {:id (user "id"),
   :followers_count (user "followers_count"), :online (user "online"), :country (user "country"), :city (user "city"), :full_name (user "full_name"),
   :track_count (user "track_count") :plan (user "plan"), :followings_count (user "followings_count"), :last_modified (user "last_modified")
   })

(defn user_parameter [user parameter]
  (user parameter))

;; (user_map (parsedusers 0))

(defn get_all_followers [userid offset acc]

  (let [call ((SCcall100 userid "followers" offset):body)

        parsed ((parse-string call) "collection")
;;         _ (println "offset" offset)

        ]
    (if (or (= call "{\"collection\":[]}") (= call nil)) acc
      (get_all_followers userid (+ offset 100) (into acc parsed)))))

(defn get_followers [userid offset]
  (let [call_rough (SCcall100 userid "followers" offset)
;;         _ (println call_rough)
        ]
        (if (= call_rough nil)
          (do (spit (str path "error") (str userid offset "\n") :append true) "no_more_followers")
          (let [call (call_rough :body)


        parsed ((parse-string call) "collection")
        filename (str path userid "_" offset)]
     (if (= call "{\"collection\":[]}")
       "no_more_followers"
        (spit filename (with-out-str (pr parsed))))))))


;; (get_followers 107508374 200)

(defn join_followers [userid]
  (loop [offset 0, acc []]
    (let [filename (str path userid "_" offset)
          try_followers (try (slurp filename)
                          (catch Exception e))]
      (if (= try_followers nil) acc
        (recur (+ offset 100) (into acc (read-string try_followers)))))))

;; (count (join_followers 107508374))
;; (spit "/Users/michal/sc/resources/SC/test.edn" (with-out-str (pr (join_followers 107508374))))

(defn delete_partial_files [userid]
  (loop [offset 0]
    (let [filename (str path userid "_" offset)]
      (if (try (io/delete-file filename) (catch Exception e))


        (recur (+ offset 100))
        "deleted"))))

(def counter (atom 1))



(defn get_all_users_followers [userid]
  (loop [offset 0]
    (let [followers (get_followers userid offset)]
      (if (= followers "no_more_followers")
        (do (spit (str path userid ".edn") (with-out-str (pr (join_followers userid))))
          (spit (str path "log") (str userid "\n") :append true)
          (println "downloaded " (str @counter) (str userid))
          (swap! counter inc)
          (delete_partial_files userid))
        (recur (+ offset 100))))))

;; (get_all_users_followers 107508374)

;; (mapv (fn [x] ((user_map x) :id)) (read-string (slurp "/Users/michal/sc/resources/SC/107508374.edn")))

;; (slurp "/Users/michal/sc/resources/SC/log")
;; (time (contains? (reduce #(conj %1 %2) (hash-set) (re-seq #"\w+" (slurp "/Users/michal/sc/resources/SC/log"))) "614112"))
;; (filter #(not (contains? hs (str %))) ls)


(defn get_all_followers_of_users_followers [userid]
  (let [already_downloaded (reduce #(conj %1 %2) (hash-set) (re-seq #"\w+" (slurp "/Users/michal/sc/resources/SC/log")))
        followers_list (mapv (fn [x] ((user_map x) :id)) (read-string (slurp (str path userid ".edn"))))
        followers_not_yet_downloaded (filter #(not (contains? already_downloaded (str %))) followers_list)
        _ (println "how many followers: " (count followers_list) "difference: " (- (count followers_list) (count followers_not_yet_downloaded)) "how many to download: " (count followers_not_yet_downloaded))

        ]
    (doall (map #(get_all_users_followers %) followers_not_yet_downloaded))))




;; (try (io/delete-file (str path "51406744_800"))
;;   (catch Exception e))



;; (def already_downloaded (reduce #(conj %1 %2) (hash-set) (re-seq #"\w+" (slurp "/Users/michal/sc/resources/SC/log"))))
;; (def error_list (re-seq #"\w+" (slurp (str path "error"))))
;; error_list

;; (filter #(not (contains? already_downloaded (str %))) error_list)

;; (def followers_list (mapv (fn [x] ((user_map x) :id)) (read-string (slurp (str path 107508374 ".edn")))))
;; followers_list

;; (filter #(not (contains? already_downloaded (str %))) followers_list)
;; (contains? already_downloaded 688425010)
;; (count already_downloaded)

;; (if (try (io/delete-file (str path "51406744_100"))
;;   (catch Exception e)) 0 11)
;; (delete_partial_files 60487226)
;; (doall (map (fn [x] (get_all_users_followers x)) [161870576 10283327 84295889]))

;; (doall (map #(get_all_users_followers %) [161870576 10283327 84295889]))
;;
;; run below with added option checking if already downloaded
;;



;; (defn -main
;;   [& args]
;;   (println "Let's Download Soundcloud!")

;; (time (do (println "enter user id: ") (get_all_followers_of_users_followers (read-line)))))


(time (do (println "enter user id: ") (get_all_followers_of_users_followers (read-line))))


;; (spit "/Users/michal/sc/resources/SC/log" "raz\n" :append true)

;; (slurp "/Users/michal/sc/resources/SC/107508374_2")

;; (try
;;                     (slurp "/Users/michal/sc/resources/SC/107508374_1")
;;                     (catch Exception e))

;; (read-string (slurp "/Users/michal/sc/resources/SC/107508374_0"))
;; (defn get_all_users_followers [userid]
;;   (loop [offset 0]
;;     (


;; (def allbojans (get_all_followers 12078 0 []))

;; (def wawas (get_all_followers wawa 0 []))
;; (def djkuba (get_all_followers 3937933 0 []))
;; (spit "/Users/michal/sc/resources/djkuba.edn" (with-out-str (pr djkuba)))
;; (spit "/Users/michal/sc/resources/wawas.edn" (with-out-str (pr wawas)))

;; (def what-is-the-answer-to-life (future
;;         (println "[Future] started computation")
;;         (Thread/sleep 3000) ;; running for 3 seconds
;;         (println "[Future] completed computation")
;;         42))

;; (time (Thread/sleep 3000))
;; (mapv (fn [x] ((user_map x) :id)) allbojans)
;; (def allbojans (read-string (slurp "/Users/michal/sc/resources/allbojans.edn")))
;; (def wawa (read-string (slurp "/Users/michal/sc/resources/wawas.edn")))
;; (def djkuba (read-string (slurp "/Users/michal/sc/resources/djkuba.edn")))


(defn nonils [followers i acc]
  (if (= i (count followers)) (sort acc)
    (if (= (followers i) nil) (recur followers (inc i) acc)
      (recur followers (inc i) (conj acc (followers i))))))

(defn separatecountries [followers acc previous i]
  (if (= followers []) acc
    (if (= (first followers) previous) (recur (rest followers) acc previous (inc i))
      (recur (rest followers) (conj acc [previous i] ) (first followers) 1))))

;; (nonils (mapv (fn [x] ((user_map x) :country)) allbojans) 0 [])


;; (spit "/Users/michal/sc/resources/allbojans_countries.edn" (with-out-str (pr (mapv (fn [x] ((user_map x) :country)) allbojans))))

;; (def allbojans_countries (read-string (slurp "/Users/michal/sc/resources/allbojans_countries.edn")))
;; (count (filter (fn [x] (and (not= x nil) (= x "Canada"))) (sort allbojans_countries)))
;; (sort allbojans_countries)

;; wawa
;; (defn usercountries [user] (separatecountries (sort (mapv (fn [x] ((user_map x) :country)) (get_all_followers user 0 []))) [] nil 1))
;; (usercountries 1763025)

;; (def allbojansinfo  (sort (mapv (fn [x] ((user_map x) :country)) allbojans)))
;; (def djkubainfo  (sort (mapv (fn [x] ((user_map x) :country)) djkuba)))
;; (def wawainfo  (sort (mapv (fn [x] ((user_map x) :country)) wawa)))
;; (def bojanfollowers  (mapv (fn [x] ((user_map x) :id)) allbojans))
;; (def wawafollowers  (mapv (fn [x] ((user_map x) :id)) wawa))
;; bojanfollowers
;; wawafollowers
;; (reduce (fn [x y] (= x y)) allbojansinfo)
;; (count wawainfo)
;; (count allbojansinfo)
;; (count (filter (fn [x] (and (not= x nil) (= x "Canada"))) wawacountries))

;; (nonils (mapv (fn [x] ((user_map x) :country)) allbojansinfo) 0 [])



;; (separatecountries allbojansinfo [] nil 1)
;; (separatecountries wawainfo [] nil 1)
;; (separatecountries djkubainfo [] nil 1)


(defn to-map [v]
  {:id (read-string (subs (v 0)5)), :username (subs (v 3)13 (dec (count (v 3)))),
   :last_modified (subs (v 4)19), :country (subs (v 8)11), :full_name (subs (v 11) 14),
   :city (subs (v 13) 9), :onlie (subs (v 18) 10), :track_count (read-string (subs (v 19) 15))
   :plan (subs (v 21) 9 (dec (count (v 21)))), :public_favs (read-string (subs (v 22) 26)),
   :followers_count (read-string (subs (v 23) 19)), :followings_count (read-string (subs (v 24) 20))})


