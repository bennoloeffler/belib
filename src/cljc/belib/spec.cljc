(ns belib.spec
  (:require
    [belib.core :refer [next-local-id]]
    #?(:clj [belib.test :refer [expect-ex]]
       :cljs [belib.test :refer-macros [expect-ex]])
    [hyperfiddle.rcf :refer [tests]]
    ;[belib.date-time :as bdt]
    [clojure.spec.alpha :as s]
    [clojure.test.check.generators :as gen]
    [clojure.string :as str]
    [tick.core :as t]
    #?(:cljs [java.time :refer [LocalDateTime LocalDate]]))
  #?(:clj
     (:import [java.time LocalDateTime LocalDate])))

(hyperfiddle.rcf/enable! false)

;;-----------------------------------------------
;; simple id
;;-----------------------------------------------
(defn gen-local-id []
  (gen/fmap (fn [dummy](next-local-id))
            (s/gen pos-int?)))

(s/def :belib/local-id
  (s/with-gen (s/or :int (s/and int? pos?) :string string?)
              gen-local-id))

(comment
  (gen-local-id)
  (s/gen :belib/local-id)
  (gen/generate (gen-local-id))
  (gen/sample (s/gen :belib/local-id)))

;;-----------------------------------------------
;; String
;;-----------------------------------------------

(s/def :belib/bulk-space-string
  (s/with-gen
    (s/and string? #(re-find #"\t|\n|\r|  +" %))
    #(gen/fmap
       str/join
       (gen/vector
         (gen/one-of
           [gen/string
            (gen/return "\t")
            (gen/return "\n")
            (gen/return "\r")
            (gen/return (str/join (repeat (rand-int 20) " ")))])))))

(comment
  (s/valid? :belib/bulk-space-string "  ")
  (gen/generate (s/gen string?))
  (gen/sample (s/gen string?) 10)
  (gen/generate (s/gen :belib/bulk-space-string)))

;;-----------------------------------------------
;; Email
;;-----------------------------------------------

(def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
(defn email? [s]
  (re-matches email-regex s))

(defn gen-email
  []
  (gen/fmap
    (fn [[name host tld]]
      (str name "@" host "." tld))
    (gen/tuple
      (gen/not-empty gen/string-alphanumeric)
      (gen/not-empty gen/string-alphanumeric)
      (gen/fmap
        #(apply str %)
        (gen/vector gen/char-alpha 2 63)))))

(comment
  (gen/sample (gen-email)))

(comment
  (email? "abc@ab.de")
  (email? "abcab.de"))

(s/def :belib/email (s/with-gen
                      (s/and string? email?)
                      gen-email))

(comment
  (gen/generate (s/gen :belib/email))
  (apply max (map count (gen/sample (s/gen :belib/email) 1000))))

(tests
  (s/valid? :belib/email "bel@gmx.de") := true
  (s/valid? :belib/email "belgmx.de") := false
  (->> (gen/sample (s/gen :belib/email) 10)
       (map #(s/valid? :belib/email %))
       (filter false?)
       (count)) := 0)

;;-----------------------------------------------
;; Percent
;;-----------------------------------------------

(defn gen-percent
  []
  (gen/fmap
    #(double (/ % 100))
    (s/gen (s/int-in 0 10001))))

(s/def :belib/percent (s/with-gen
                        (s/and number? #(>= % 0) #(<= % 100))
                        gen-percent))

(tests
  (s/valid? :belib/percent 101) := false
  (s/valid? :belib/percent -1) := false
  (s/valid? :belib/percent "50") := false
  (s/valid? :belib/percent 100) := true
  (s/valid? :belib/percent 0) := true
  (s/valid? :belib/percent 0.0) := true
  (s/valid? :belib/percent 100.0) := true
  (s/valid? :belib/percent 50) := true)

(comment
  (gen/generate (s/gen :belib/percent))
  ;; about every 10000th is 100%. So in 50.000 there should be about avg 5...
  (filter #(= 100.00 %) (gen/sample (gen-percent) 50000)) ; => rare... but sometimes
  (gen/sample (s/gen (s/int-in 0 10))))

#_(tests
    (filter #(or (< % 0.0) (> % 100.0)) (gen/sample (gen-percent) 10000)) := []
    (pos? (count (filter #(= % 0.0) (gen/sample (gen-percent) 10000)))) := true
    (pos? (count (filter #(= % 100.0) (gen/sample (gen-percent) 90000)))) := true
    (count (filter #(= % 101.0) (gen/sample (gen-percent) 10000))) := 0)

;;-----------------------------------------------
;; t/date
;;-----------------------------------------------

; 2010-01-04 until 2039-12-31 (including)
(def start-epoch-day (.toEpochDay (t/date "2010-01-04")))
(def end-epoch-day (.toEpochDay (t/date "2040-01-01")))

(defn gen-local-date
  "Generates LocalDate starting from
  2010-01-04 until 2039-12-31 (including)."
  []
  (gen/fmap
    (fn [day] (t/new-date ^long day))

    ; epoch days from 1970 to 2070
    #_(s/gen (s/int-in 0 (* 365 100)))

    ; 2010-01-04 until 2039-12-31 (including)
    (s/gen (s/int-in (.toEpochDay (t/date "2022-01-04")) #_start-epoch-day
                     (.toEpochDay (t/date "2024-01-04")) #_end-epoch-day))))


(s/def :belib/local-date
  (s/spec
    (fn is-instance-of-LocalDate [date]
      (and (instance? LocalDate date)
           (>=  (.toEpochDay date) start-epoch-day)
           (<  (.toEpochDay date) end-epoch-day)))
    :gen gen-local-date))

(declare validate)

(comment
  (validate :belib/local-date (t/date "2023-03-04"))
  (validate :belib/local-date (t/date))
  (validate :belib/local-date :something-else)
  (gen/generate (s/gen :belib/local-date)))

(tests
  (->> (gen/sample (s/gen :belib/local-date) 100)
       (map #(s/valid? :belib/local-date %))
       (filter false?)) := []
  nil)

;;-------------------------------------------------
;; Helpers
;;-------------------------------------------------

(defn validate
  "Validates and returns validated value
  according to a spec. Otherwise throws
  ExceptionInfo with result of explain-str.
  May be used to validate specs in
  pre- and post-conditions.
  Caution: nil may be a valid value according
  to spec but may fail for :pre and :post!

  Example:
  (s/def :belib/test-int int?)
  (defn test-validate [n]
    {:pre [(validate :belib/test-int n)]}
    (* n n))

  Tests that should fail can be done with
  macro expect-ex:
  (expect-ex (test-validate 2.0))
  It will return the thrown Throwable as value:
  (is (=
        (expect-ex (test-validate 2.0))
        clojure.lang.ExceptionInfo))"
  [spec value]
  (let [explanation (s/explain-str spec value)]
    (if (= explanation "Success!\n")
      value
      (throw (ex-info explanation {:value value})))))



