(ns sc.follower
   (:require [clj-http.client :as client]
             [clojure.data.json :as json]
             [crouton.html :as html]
             [cheshire.core :refer :all]

             [clojure.string]
             [sc.core :refer :all]))




(sort bojanfollowers)
(sort wawafollowers)
(hash-map 2012345 nil)
(defn addtohasmap [x]
  (conj (hash-map x nil)))

(defn hashfollowers [followers acc]
  (if (empty? followers) acc
    (recur (rest followers) (conj acc (hash-map (first followers) false)))))

(hash-map (first bojanfollowers) false)
(def bojan_followers_hashmap (hashfollowers (sort bojanfollowers) (hash-map 0 false)))
(assoc bojan_followers_hashmap 0 true)
(bojan_followers_hashmap 0)

(defn check_dup_followers [hashedfollowers S2]
  (if (empty? S2) hashedfollowers
    (if (= (hashedfollowers (first S2)) nil)
      (recur hashedfollowers (rest S2))

      (let [new_hashedfollowers (assoc hashedfollowers (first S2) true)]
        (recur new_hashedfollowers S2)))))


(defn same_followers [hashedfollowers followers]
  (if (empty? followers) hashedfollowers
    (if (nil? (hashedfollowers (first followers))) (recur hashedfollowers (rest followers))
      (recur (assoc hashedfollowers (first followers) true) (rest followers)))))


(same_followers bojan_followers_hashmap wawafollowers)

(count (filter (fn [x] (= x true)) (vals (same_followers bojan_followers_hashmap wawafollowers))))
