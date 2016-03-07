(ns sc.wybory
   (:require [clj-http.client :as client]
             [clojure.string]
             [net.cgrand.enlive-html :as html]
             [cheshire.parse :as parse]
             [clojure.java.io :as io]))




(defn httpCall [url]
  (client/get url {:socket-timeout 2000 :conn-timeout 2000}))

;; reposters
(def wynik (:body (httpCall "http://parlament2015.pkw.gov.pl/321_protokol_komisji_obwodowej/97507")))
;;likers
(httpCall "http://parlament2015.pkw.gov.pl/321_protokol_komisji_obwodowej/97507")

;; (httpCall "http://sh191485.website.pl/joe/")

(parse/parse-stream wynik)


(defn got404? [userid method offset] (= (try (client/get "http://parlament2015.pkw.gov.pl/321_protokol_komisji_obwodowej/97507" {:socket-timeout 2000 :conn-timeout 2000})
  (catch Exception e (str (.getMessage e)))) "clj-http: status 404"))




;; (defn -main []
;;   (do (println "enter user id or ids: ")
;;     (let [ids (map read-string (re-seq #"\w+" (read-line)))]
;;       (time (doall (map id_sucker ids))))))

