(ns belib.date-parse-messy
  (:require [belib.core :as bc]
            [belib.date-time :as bd]
            [tick.core :as t]
            [hyperfiddle.rcf :refer [tests]]
            #?(:cljs [cljs.core :refer [ExceptionInfo]])
            #?(:clj  [belib.test :as bt :refer [expect-ex return-ex]]
               :cljs [belib.test :as bt :refer-macros [expect-ex return-ex]]))
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(hyperfiddle.rcf/enable! false)

(defn detect-date-format
  "iso date: 1997-1-27 (you may leaf out all leading 0)
   eu date:  27.1.1997 (you may leaf out all leading 0)
   us date:  1/27/1997 (you may leaf out all leading 0)
   iso week: 2003-W01  (you may leaf out all leading 0)

   return the data found and the format, e.g.:
   [\"2026-W3\" :iso-week]
   [\"2026-12-24\" :iso-date]
   [\"3.4.2026\" :eu-date]
   [\"4/21/2026\" :us-date]
   or
   nil, if no date format was found."
  [time-str]
  (let [re         (bc/long-re #"(?<isoweek>^\d{4}-[Ww]\d{1,2}$)|"
                               #"(?<isodate>^\d{4}-\d{1,2}-\d{1,2}$)|"
                               #"(?<eudate>^\d{1,2}\.\d{1,2}\.\d{4}$)|"
                               #"(?<usdate>^\d{1,2}/\d{1,2}/\d{4}$)")
        match      (re-matches re time-str)
        match-type (when match (cond (get match 1) :iso-week
                                     (get match 2) :iso-date
                                     (get match 3) :eu-date
                                     (get match 4) :us-date
                                     :else :error))]
    (when match [(get match 0) match-type])))


(tests

  "no date format"
  (detect-date-format "2026--12") := nil
  (detect-date-format "202.6.12") := nil
  (detect-date-format "2026-x1-1") := nil

  "iso-date"
  (detect-date-format "2026-3-12") := ["2026-3-12" :iso-date]
  (detect-date-format "2026-12-24") := ["2026-12-24" :iso-date]

  "iso-week"
  (detect-date-format "2026-W3") := ["2026-W3" :iso-week]
  (detect-date-format "2026-W12") := ["2026-W12" :iso-week]
  (detect-date-format "2026-W03") := ["2026-W03" :iso-week]

  "eu-date"
  (detect-date-format "3.4.2026") := ["3.4.2026" :eu-date]
  (detect-date-format "01.01.2026") := ["01.01.2026" :eu-date]

  "us-date"
  (detect-date-format "3/23/2026") := ["3/23/2026" :us-date]
  (detect-date-format "01/01/2026") := ["01/01/2026" :us-date]

  :end-tests)

(defn parse-iso-week [date-str]
  (let [[_ y w] (re-matches #"^(\d{4})-[Ww](\d{1,2})$" date-str)]
    (if (and y w)
      [(bc/parse-long y) (bc/parse-long w)]
      (throw (ex-info (str "no iso week format: " date-str) {:data date-str})))))

(tests
  (parse-iso-week "2023-W1") := [2023 1]
  (parse-iso-week "2023-w01") := [2023 1]
  (expect-ex (parse-iso-week "2023-X-W1")) := ExceptionInfo

  :end-tests)

(defn parse-iso-date [date-str]
  (let [[_ y m d] (re-matches #"^(\d{4})-(\d{1,2})-(\d{1,2})$" date-str)]
    (if (and y m d)
      [(bc/parse-long y) (bc/parse-long m) (bc/parse-long d)]
      (throw (ex-info (str "no iso date format: " date-str) {:data date-str})))))

(tests
  (parse-iso-date "2023-1-02") := [2023 1 2]
  (expect-ex (parse-iso-date "2023-p1-02")) := ExceptionInfo

  :end-tests)

(defn parse-eu-date [date-str]
  (let [[_ d m y] (re-matches #"^(\d{1,2})\.(\d{1,2})\.(\d{4})$" date-str)]
    (if (and y m d)
      [(bc/parse-long y) (bc/parse-long m) (bc/parse-long d)]
      (throw (ex-info (str "no eu date format: " date-str) {:data date-str})))))

(tests
  (parse-eu-date "2.1.2023") := [2023 1 2]
  (expect-ex (parse-eu-date "23.1.2.2023")) := ExceptionInfo

  :end-tests)

(defn parse-us-date [date-str]
  (let [[_ m d y] (re-matches #"^(\d{1,2})/(\d{1,2})/(\d{4})$" date-str)]
    (if (and y m d)
      [(bc/parse-long y) (bc/parse-long m) (bc/parse-long d)]
      (throw (ex-info (str "no us date format: " date-str) {:data date-str})))))

(tests
  (parse-us-date "2/21/2023") := [2023 2 21]
  (expect-ex (parse-us-date "23/1/2.2023")) := ExceptionInfo

  :end-tests)

(defn transform-to-data [date-str]
  (let [match-to-data (fn [match-data]
                        (let [type (second match-data)
                              data (first match-data)]
                          (condp = type
                            :iso-week (parse-iso-week data)
                            :iso-date (parse-iso-date data)
                            :eu-date (parse-eu-date data)
                            :us-date (parse-us-date data))))]
    (if-let [match (detect-date-format date-str)]
      (match-to-data match)
      (throw (ex-info (str "unknown date format: " date-str) {:data date-str})))))

(tests
  (transform-to-data "2023-W25") := [2023 25]
  (transform-to-data "2023-5-11") := [2023 5 11]
  (transform-to-data "2.1.2023") := [2023 1 2]
  (transform-to-data "2/1/2023") := [2023 2 1]

  (expect-ex (transform-to-data "a2.1.2023")) := ExceptionInfo

  :end-tests)

(defn transform-to-date [data]
  (let [[y m d] data]
    (if d
      (t/new-date y m d)
      (bd/monday-from-year-week [y m]))))

(tests
  "parse messy, different week formats"
  (-> "2023-W39" transform-to-data transform-to-date) := (t/date "2023-09-25")
  (-> "2023-W1" transform-to-data transform-to-date) := (t/date "2023-01-02")
  (-> "2023-W01" transform-to-data transform-to-date) := (t/date "2023-01-02")

  "parse messy iso dates"
  (-> "2023-12-31" transform-to-data transform-to-date) := (t/date "2023-12-31")
  (-> "2023-1-01" transform-to-data transform-to-date) := (t/date "2023-01-01")

  "parse mess eu dates"
  (-> "31.12.2023" transform-to-data transform-to-date) := (t/date "2023-12-31")
  (-> "1.01.2023" transform-to-data transform-to-date) := (t/date "2023-01-01")

  "parse messy us dates"
  (-> "12/31/2023" transform-to-data transform-to-date) := (t/date "2023-12-31")
  (-> "12/1/2023" transform-to-data transform-to-date) := (t/date "2023-12-01")
  (-> "12/01/2023" transform-to-data transform-to-date) := (t/date "2023-12-01")

  "too messy - wrong"
  (ex-message (return-ex (-> "blah12/31/2023" transform-to-data transform-to-date)))
  := "unknown date format: blah12/31/2023"

  :end-tests)

(defn parse-messy-date
  "Parse different string formats:
   iso date: 1997-1-27 (you may leaf out all leading 0)
   eu date:  27.1.1997 (you may leaf out all leading 0)
   us date:  1/27/1997 (you may leaf out all leading 0)
   iso week: 2003-W01  (you may leaf out all leading 0)

   returns: tick/date."
  [messy-date-str]
  (-> messy-date-str transform-to-data transform-to-date))

(tests
  (defn test-parse-messy-date [pairs]
    (mapv (fn [[data result]] (parse-messy-date data) := result) pairs))
  (test-parse-messy-date [["2023-W39" (t/date "2023-09-25")]
                          ["2023-W1" (t/date "2023-01-02")]
                          ["2023-W01" (t/date "2023-01-02")]
                          ["2023-w01" (t/date "2023-01-02")]
                          ["2023-12-31" (t/date "2023-12-31")]
                          ["2023-1-01" (t/date "2023-01-01")]
                          ["31.12.2023" (t/date "2023-12-31")]
                          ["1.01.2023" (t/date "2023-01-01")]
                          ["12/31/2023" (t/date "2023-12-31")]
                          ["12/1/2023" (t/date "2023-12-01")]
                          ["12/01/2023" (t/date "2023-12-01")]])

  "too messy - wrong"
  (ex-message (return-ex (-> "blah12/31/2023" parse-messy-date)))
  := "unknown date format: blah12/31/2023"

  :end-tests)