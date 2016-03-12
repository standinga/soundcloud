(ns sc.pings
   (:require [clj-http.client :as client]
             [cheshire.core :refer :all]
             [clojure.string]
             [net.cgrand.enlive-html :as html]
             [cheshire.core :refer :all]
             [clojure.java.io :as io]
             ))




(defn ping [x]
  (if
    (try (.isReachable (java.net.InetAddress/getByName (str "192.168.1." x )) 500)
    (catch Exception e false))
    (println (str "host: 192.168.1." x " is OK "))))





(defn -main []
  (doall (pmap ping (range 255))))


;; (time (-main))

(-main)
