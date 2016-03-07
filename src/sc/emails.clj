(ns sc.emails
   (:require [clj-http.client :as client]
             [clojure.data.json :as json]
             [crouton.html :as html]
             [cheshire.core :refer :all]
             [clojure.string]
             [clojure.java.io :as io]
;;              [sc.core :refer :all]
             ))


(defn read-lines [filename]
  (with-open [rdr (io/reader filename)]
  (doall (line-seq rdr))))

(def badchars (hash-set \/ \? \! \' \; \" \& \) \( \ \: \, \. \  \' \∆ \∞ \▬ \# \% \@ \*))
(def lessbadchars (hash-set \/ \? \! \' \; \" \& \) \( \ \: \  \, \' \∆ \∞ \▬ \# \% \*))
(def numbers (hash-set \0 \1 \2 \3 \4 \5 \6 \7 \8 \9))

(defn isnumber? [chara] (contains? numbers chara))

(defn badchar? [chara] (contains? badchars chara))

(defn lessbadchar? [chara] (contains? lessbadchars chara))

(def emailsfile (str path1 "emails.txt"))

(defn get_user_id [user_name]
  "takes name of user and returns user id"
  (let [call (httpCall (str "http://api.soundcloud.com/users/" user_name "?client_id=" soundcloud_client_id))
        parsed_call (parse-string (:body call))]
    (get parsed_call "id")))

(defn hashfollowers [followers acc]
  (if (empty? followers) acc
    (recur (rest followers) (conj acc (hash-map (first followers) false)))))

(defn nonils [followers]
  (loop [i 0 acc []]
  (if (= i (count followers)) (sort acc)
    (if (= (followers i) nil) (recur  (inc i) acc)
      (recur  (inc i) (conj acc (followers i)))))))

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

(defn read-file [userid]
  (read-string (slurp (str path userid ".edn"))))


(defn followers_descriptions [userid]
  "gets descriptions of followers downloaded"
  (filterv #(not= % "") (nonils (mapv (fn [x] ((user_map x) :description)) (read-file userid)))))


(defn extract_mail [description]
  (vec (re-seq #"((\w+)(\p{Punct})(\w+)@(\w+)(\p{Punct})(\w+)|(\w+)@(\w+)(\p{Punct})(\w+)|(\w+)@(\w+)(\p{Punct})(\w+)(\p{Punct})(\w+)|(\w+)(\p{Punct})(\w+)@(\w+)(\p{Punct})(\w+)(\p{Punct})(\w+)
               |(\w+)(\p{Punct})(\w+)@(\w+)(\p{Punct})(\w+)(\p{Punct})(\w+)(\p{Punct})(\w+))"
        description)))


(defn extract_mail2 [description]
  (reduce conj [] (re-seq #"\S+@\S+"
               description)))

(defn extract_mail3 [description]
  "extract email from string this one is used in main function"
  (reduce conj [] (re-seq #"\S+@\S+|\{(?:\w+, *)+\w+\}@[\w.-]+"
               description)))


(defn remove_less_bad
  "aux function for nobreaks"
  [string acc]
  (if (< (count string) 7)
    "badaddress"
  (if (= (get string 0) \@)
         (str acc string)
    (if (lessbadchar? (get string 0))
      (recur (subs string 1) "")
      (recur (subs string 1) (str acc (get string 0)))))))

(defn nobreaks [string]
  "leaves only valid email address without booking: contact: etc."
  (remove_less_bad string ""))

(defn check_last_letter [string]
  "keeps checking last letter of address until valid letter, removes . , ; etc from the end of address"
  (let [length (count string)]
    (if (< length 7)
      "bad address"
      (if (or (badchar? (get string (dec length))) (isnumber? (get string (dec length))))
           (recur (subs string 0 (dec length)))
           string))))

(defn check_first_letter [string]
  "removes from the  beginning of email address  , . ( etc. "
  (if (< (count string) 7) "bad address"
    (if (badchar? (get string 0))
      (recur (subs string 1))
      string)))


(defn check_first_last [string]
  "checks address for and illegal characters and cleans beginning and end of address"
  (nobreaks (check_first_letter (check_last_letter string))))


(defn extract_emails [userid]
  "extract all available email addresses of user's followers"
  (let [user_followers_desrciptions (followers_descriptions userid)
        mails_no_nils (filterv #(not= % []) (mapv extract_mail3 user_followers_desrciptions))]
     (filterv #(not= % "badaddress" ) (mapv check_first_last (mapv #(% 0) mails_no_nils)))))


(defn add_emails
  "extracts emails and add them to email.txt file, discards duplicates"
  [useridorname]
  (let [userid (if (string? useridorname) (get_user_id useridorname) useridorname)
        extracted_emails (reduce conj (hash-set) (extract_emails userid))
        _ (println (str "extracted " (count extracted_emails) " email addresses"))
        already_extracted_emails (reduce conj (hash-set) (read-lines emailsfile))
        emails_not_yet_added (filter #(not (contains? already_extracted_emails (str %))) extracted_emails)
        _ (println (str "emails already extracted: " (count already_extracted_emails) ", emails to be added: " (count emails_not_yet_added)))]
    (doall (map #(spit emailsfile (str % "\n") :append true) (sort emails_not_yet_added)))))




;; (get_all_users_followers (get_user_id "gleb-shmelkov"))

;; (add_emails "gleb-shmelkov")


;; (pmap #(get_all_users_followers (get_user_id %)) ["futurehousebombs" "jochenpash" "davidnoakes" "patrickhagenaar" "imminentred"  "mightyfools" "wearefuturistic" "thomashayden" "nickmorenaofficial" "future-house-basterds" "liamkeegan" "thefuturehouse" "crazibiza" "2elementsdjs" "bangitrecords" "chocolate-puma" "hexagon" "houseshaker" "mydigitalenemy" "slidebackmusic" "plastikfunk" "futurehouseanddeep" "fresh-musique" "futures-finest" "future-drops" "nuhousepromo" "futurehouserecords" "scissors-music" "bmr-edmlead" "heldeepradio" "fokus-official" "izydj" "spinninrecords" "heldeepradio" "oliverheldens" "housesession-records" "tune-brothers" "dondiablo" "dj-sign" "futurehouserecords" "djronslomowicz" "hi-lomusic" "futurehouseanddeep"  "danylmusic" "jonas-aden" "lucasluckonline" "futurehousesound" ])
;; (map #(add_emails %) ["futurehousebombs" "jochenpash" "davidnoakes" "patrickhagenaar" "imminentred"  "mightyfools" "wearefuturistic" "thomashayden" "nickmorenaofficial" "future-house-basterds" "liamkeegan" "thefuturehouse" "crazibiza" "2elementsdjs" "bangitrecords" "chocolate-puma" "hexagon" "houseshaker" "mydigitalenemy" "slidebackmusic" "plastikfunk" "futurehouseanddeep" "fresh-musique" "futures-finest" "future-drops" "nuhousepromo" "futurehouserecords" "scissors-music" "bmr-edmlead" "heldeepradio" "fokus-official" "izydj" "spinninrecords" "heldeepradio" "oliverheldens" "housesession-records" "tune-brothers" "dondiablo" "dj-sign" "futurehouserecords" "djronslomowicz" "hi-lomusic" "futurehouseanddeep"  "danylmusic" "jonas-aden" "lucasluckonline" "futurehousesound" ])
