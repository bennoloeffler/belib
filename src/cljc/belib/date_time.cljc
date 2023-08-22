(ns belib.date-time
  (:require
    ;[clojure.test :as t]
    [tick.core :as t]
    [cljc.java-time.temporal.chrono-unit :as cu]
    [hyperfiddle.rcf :refer [tests]]
    [tick.alpha.interval :as tai]
    [belib.core :as bc]
    #?@(:cljs [[goog.string :as gstring]
               [java.time :refer [LocalDateTime LocalDate]]

               [belib.test :refer-macros [expect-ex]]])
    #?@(:clj [[belib.test :refer [expect-ex]]
              [java-time.api :as jt]]))
  #?(:clj
     (:import [java.time ZoneOffset LocalDateTime LocalDate ZonedDateTime]
              [java.util Date])))


(hyperfiddle.rcf/enable! false)


(def d t/date)
(def dt t/date-time)


(defn date?
  "is it a LocalDate (js and jvm)"
  [date]
  (= LocalDate (type date)))

(defn date-time?
  "is it a LocalDateTime (js and jvm)"
  [date-time]
  (= LocalDateTime (type date-time)))

(comment
  (= LocalDateTime (type (dt "2003-01-01T00:00")))
  (instance? LocalDateTime (dt "2003-01-01T00:00")))

(tests
  (date? (d "2003-01-01")) := true
  (date? (dt "2003-01-01T00:00")) := false
  (date-time? (dt "2003-01-01T00:00")) := true
  (date-time? (d "2003-01-01")) := false)


(defn ymd-hms-vec [tick-date-time]
  "Vector [y m d] of a tick date."
  (let [year   (t/int (t/year tick-date-time))
        month  (t/int (t/month tick-date-time))
        day    (t/day-of-month tick-date-time)
        hour   (t/hour tick-date-time)
        minute (t/minute tick-date-time)
        second (t/second tick-date-time)]
    [year month day hour minute second]))

(tests
  (ymd-hms-vec (t/date-time "2027-01-03T21:23:12")) := [2027 1 3 21 23 12])

(defn ymd-vec [tick-date]
  "Vector [y m d] of a tick date."
  (let [year  (t/int (t/year tick-date))
        month (t/int (t/month tick-date))
        day   (t/day-of-month tick-date)]
    [year month day]))

(tests
  (ymd-vec (t/date "2027-01-03")) := [2027 1 3])


(defn date-to-str [tick-date]
  "do simple formatting - also cljs - without having
  required [tick.locale-en-us]"
  (let [[y m d] (ymd-vec tick-date)]
    (#?(:cljs gstring/format
        :clj  format)
      "%4d-%02d-%02d" y m d)))

(tests
  (date-to-str (t/date "2027-01-03")) := "2027-01-03")

(defn date-time-to-str [tick-date-time]
  "do simple formatting - also cljs - without having
  required [tick.locale-en-us]"
  (let [[y m d h mi s] (ymd-hms-vec tick-date-time)]
    (#?(:cljs gstring/format
        :clj  format)
      "%4d-%02d-%02d %02d:%02d:%02d" y m d h mi s)))

(tests
  (date-time-to-str (t/date-time "2027-01-03T21:22:23")) := "2027-01-03 21:22:23")


#?(:cljs
   (defn tick-date-to-native-date [tick-date]
     (let [year  (t/int (t/year tick-date))
           month (t/int (t/month tick-date))
           day   (t/day-of-month tick-date)]
       (js/Date. (js/Date.UTC year (dec month) day 0 0 0 0)))))

#?(:clj
   (defn tick-date-to-native-date [tick-date]
     (let [[y m d] (ymd-vec tick-date)
           ldt (LocalDateTime/of y m d 0 0)
           zdt (.atZone ldt ZoneOffset/UTC)]
       (Date/from (.toInstant zdt)))))

(tests
  (some? (tick-date-to-native-date (t/date "2027-01-03"))) := true
  (tick-date-to-native-date (t/date "2027-01-03")) := #inst"2027-01-03T00:00:00Z")

(defn list-of-all-days [start end]
  (map t/date
       (t/range
         start
         end
         (t/new-period 1 :days))))

(tests
  (count (list-of-all-days
           (dt "2027-01-01T00:00")
           (dt "2027-01-05T00:00"))) := 4)

(defn duration-in-days
  "Start and end are dates.
  Returns whole days.
  Returns negative value, when end is before start."
  [start end]
  (assert (date? start))
  (assert (date? end))
  (cu/between cu/days start end)
  #_(let [days (fn [s e] (dec (t/days (t/duration (tai/new-interval s e)))))]
      (if (t/< end start)
        (- (days end start))
        (days start end))))

; TODO use this
(comment
  (cu/between cu/days (d "2023-08-31") (d "2023-09-01"))
  (cu/between cu/days (d "2023-09-30") (d "2023-09-29")))


(tests
  (duration-in-days (d "2023-01-01") (d "2023-01-02")) := 1
  (duration-in-days (d "2023-01-01") (d "2023-01-01")) := 0
  (duration-in-days (d "2023-01-02") (d "2023-01-01")) := -1
  (duration-in-days (d "2024-01-01") (d "2023-01-01")) := -365)

(defn duration
  "start and end are date-times: (dt \"2023-01-01T00:00\").
  Default for t-fun is t/millis.
  t-fun is the function to extract the unit of the duration:
  t/days, t/seconds, t/millis, etc."
  ([start end]
   (duration start end t/millis))
  ([start end t-fun]
   (assert (date-time? start))
   (assert (date-time? end))
   (assert (t/<= start end) "start needs to be before or equal to end")
   (if (t/= start end)
     0
     (t-fun (t/duration (tai/new-interval start end))))))


(comment
  (t/days (t/duration (tai/new-interval (d "2023-01-01") (d "2023-01-02"))))
  (t/days (t/duration (tai/new-interval (dt "2023-01-01T00:00") (dt "2023-01-02T00:00"))))
  (duration (dt "2023-01-01T00:00") (dt "2023-01-01T00:00"))
  (duration (dt "2023-01-01T00:00") (dt "2023-01-01T00:01"))
  (duration (dt "2023-01-01T00:00") (dt "2023-01-01T00:01") t/seconds)
  (duration (dt "2023-01-01T00:00") (dt "2023-01-01T00:01") t/days)
  (duration (dt "2023-01-01T00:00") (dt "2024-01-01T00:00") t/days)
  (duration (dt "2023-01-01T00:00") (dt "2024-01-06T00:00") t/days)
  ; no: (duration (dt "2023-01-01T00:00") (dt "2024-01-06T00:00") t/months)
  ; no (duration (dt "2023-01-01T00:00") (dt "2024-01-06T00:00") t/years)
  nil)


;; TEST OPITON 1
(defn test-wrong-dur-type []
  (duration 12 (dt "2023-01-01T00:01") t/seconds))

#?(:cljs (tests
           (expect-ex (test-wrong-dur-type)) := js/Error))

#?(:clj (tests
          (expect-ex (test-wrong-dur-type)) := AssertionError))

;; TEST OPITON 2
(tests
  (expect-ex (duration 12 (dt "2023-01-01T00:01") t/seconds)) := #?(:cljs js/Error :clj AssertionError))



;;----------------------------------------------------------------
;; handle iso calender week with tick
;;----------------------------------------------------------------


;; https://weeknumber.com/how-to/javascript

#?(:cljs
   (defn week-of-week-based-year [tick-date]
     (let [d (tick-date-to-native-date tick-date)]
       (.setHours d 0 0 0 0)
       (.setDate d (+ (.getDate d) 3 (- (mod (+ (.getDay d) 6) 7))))
       (let [week1 (js/Date. (.getFullYear d) 0 4)]
         (-> (- (.getTime d) (.getTime week1))
             (/ 86400000)
             (- 3)
             (+ (mod (+ (.getDay week1) 6) 7))
             (/ 7)
             Math/round
             inc)))))

#?(:cljs
   (defn week-based-year [tick-date]
     (let [d (tick-date-to-native-date tick-date)]
       (.setDate d (+ (.getDate d) 3 (- (mod (+ (.getDay d) 6) 7))))
       (.getFullYear d))))

(defn iso-week-year [tick-date]
  #?(:cljs [(week-based-year tick-date) (week-of-week-based-year tick-date)]
     :clj  (let [all-data (jt/as-map tick-date)]
             [(:week-based-year all-data)
              (:week-of-week-based-year all-data)])))

(tests
  (iso-week-year (t/date (t/instant "2014-12-31T00:00:00Z"))) := [2015 1]
  (iso-week-year (t/date "2017-01-01")) := [2016 52]
  (iso-week-year (t/date "2018-12-31")) := [2019 1]
  (iso-week-year (t/date "2019-01-01")) := [2019 1]
  (iso-week-year (t/date "2026-12-27")) := [2026 52]
  (iso-week-year (t/date "2027-01-03")) := [2026 53]
  (iso-week-year (t/date "2027-01-04")) := [2027 1]

  nil)


(defn date-breakdown
  "Takes a tick/date and breaks it down into units
  including iso week of year (week-of-week-based-year)
  and the corresponding year (week-based-year)."
  [tick-date]
  (let [[y m d] (ymd-vec tick-date)
        [week-based-year week-of-week-based-year] (iso-week-year tick-date)]

    {:month                   m
     :day                     d
     :year                    y
     :week-based-year         week-based-year
     :week-of-week-based-year week-of-week-based-year}))

(tests
  (date-breakdown (t/date "2027-01-03")) := {:month                   1,
                                             :day                     3,
                                             :year                    2027,
                                             :week-based-year         2026,
                                             :week-of-week-based-year 53})




(comment
  ;(dt (d "2027-01-03"))
  (bc/pprint (date-breakdown (t/date "2027-01-03")))
  (bc/pprint (date-breakdown (t/date "2027-01-03T00:00:00")))
  (bc/pprint (list-of-all-days (dt "2027-01-01T00:00") (dt "2027-01-05T00:00")))

  ; java.util.Date and js/Date are called insts in tick
  ; so date is ALWAYS local data

  ;; https://juxt.github.io/tick/#_api

  ;; #time/time LOCAL time
  (t/time) ; current time
  (t/time "19:45")

  ;; #time/date LOCAL date
  (t/date) (t/today)
  (t/date "2018-06-21")

  ;; #time/day-of-week
  (t/day-of-week (t/today))
  (t/day-of-week (t/tomorrow))

  ;; #time/date-time LOCAL date-time
  (t/date-time) ; current date time
  (t/date-time "1918-11-13T23:59")

  ;; #time/offset-date-time
  (t/offset-date-time "1918-11-13T11:00+01:00")

  ;; DOES NOT WORK in CLJS - not pre-packaged,
  ;; see: https://juxt.github.io/tick/#_timezones
  (t/zoned-date-time
    "1918-11-11T11:00:00Z[Europe/Paris]")
  ;; #time/zoned-date-time
  (t/zoned-date-time
    "1918-11-11T11:00:00Z")

  ;; #time/instant
  (t/instant (t/offset-date-time "1918-11-11T11:00:00+01:00"))
  (t/instant) (t/now) ; UTC!
  (t/date (t/instant))
  (t/max (t/today) (t/tomorrow))

  nil)


