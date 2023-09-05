(ns belib.date-time
  (:require
    ;[clojure.test :as t]
    [tick.core :as t]
    [cljc.java-time.temporal.chrono-unit :as cu]
    [hyperfiddle.rcf :refer [tests]]
    [tick.alpha.interval :as tai]
    [belib.core :as bc]
    #?(:cljs [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break
                                                 clog_ clogn_ dbg_ dbgn_ break_]]
       :clj  [debux.cs.core :as d :refer [clog clogn dbg dbgn break
                                          clog_ clogn_ dbg_ dbgn_ break_]])
    #?@(:cljs [[goog.string :as gstring]
               [java.time :refer [LocalDateTime LocalDate]]

               [belib.test :refer-macros [expect-ex]]])
    #?@(:clj [[belib.test :refer [expect-ex]]
              [java-time.api :as jt]]))
  #?(:clj
     (:import [java.time ZoneOffset LocalDateTime LocalDate ZonedDateTime]
              [java.util Date])))

#_(:import [java.time LocalDate]
    [java.time.temporal IsoFields])

(hyperfiddle.rcf/enable! true)

(comment
  (require 'playback.core) ; open the portal
  (dbgn (+ 3 (- 4 5))))


(def d t/date)
(def dt t/date-time)


#_(defn date?
    ; TODO same as t/date? Yes: replace!
    "is it a LocalDate (js and jvm)"
    [date]
    (= LocalDate (type date)))

#_(comment
    (t/date? (d "2023-01-01")))

(defn date-time?
  "is it a LocalDateTime (js and jvm)"
  [date-time]
  (= LocalDateTime (type date-time)))

(comment
  (= LocalDateTime (type (dt "2003-01-01T00:00")))
  (instance? LocalDateTime (dt "2003-01-01T00:00")))

(tests
  (t/date? (d "2003-01-01")) := true
  (t/date? (dt "2003-01-01T00:00")) := false
  (t/date-time? (dt "2003-01-01T00:00")) := true
  (t/date-time? (d "2003-01-01")) := false)


(defn ymd-hms-vec
  "Vector [year month day  hour min second] of a tick date."
  [tick-date-time]
  (let [year   (t/int (t/year tick-date-time))
        month  (t/int (t/month tick-date-time))
        day    (t/day-of-month tick-date-time)
        hour   (t/hour tick-date-time)
        minute (t/minute tick-date-time)
        second (t/second tick-date-time)]
    [year month day hour minute second]))

(tests
  (ymd-hms-vec (t/date-time "2027-01-03T21:23:12")) := [2027 1 3 21 23 12])

(defn ymd-vec
  "Vector [year month day weekday] of a tick date."
  [tick-date]
  (let [year      (t/int (t/year tick-date))
        month     (t/int (t/month tick-date))
        day       (t/day-of-month tick-date)
        week-day  (keyword (str (t/day-of-week tick-date)))
        epoch-day (.toEpochDay tick-date)]
    [year month day week-day epoch-day]))

(tests
  (ymd-vec (t/date "2027-01-03")) := [2027 1 3 :SUNDAY 20821])


(defn date-to-str
  "do simple formatting - also cljs - without having
  required [tick.locale-en-us]"
  [tick-date]
  (let [[y m d] (ymd-vec tick-date)]
    (#?(:cljs gstring/format
        :clj  format)
      "%4d-%02d-%02d" y m d)))

(tests
  (date-to-str (t/date "2027-01-03")) := "2027-01-03")

(defn date-time-to-str
  "do simple formatting - also cljs - without having
  required [tick.locale-en-us]"
  [tick-date-time]
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
  (assert (t/date? start))
  (assert (t/date? end))
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
  #?(:cljs
     [(week-based-year tick-date) (week-of-week-based-year tick-date)]

     :clj
     ;; (.get (LocalDate/of 2023 1 1) IsoFields/WEEK_OF_WEEK_BASED_YEAR)
     ;; (.get (LocalDate/of 2023 1 1) IsoFields/WEEK_BASED_YEAR)
     (let [all-data (jt/as-map tick-date)]
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


(def first-day-of-abs-week-0
  "the definition of the origin of abs-week,
  2009-12-28, put as epoch-day: 14606"
  (.toEpochDay (d "2009-12-28")))

(defn get-abs-week
  "The absolute week increases by 1 every 7 days,
  beginning Monday, 2009-12-28 with 0.
  So 2010-01-04 is abs-week 1.
  And 2009-12-28 is abs-week -1."
  [tick-date]
  (let [days-diff (- (.toEpochDay tick-date)
                     first-day-of-abs-week-0)
        year      (t/year tick-date)
        month     (t/year tick-date)
        day       (t/day-of-month tick-date)]
    (quot days-diff 7)))

(tests
  (get-abs-week (d "2009-12-21")) := -1
  (get-abs-week (d "2009-12-28")) := 0
  (get-abs-week (d "2010-01-04")) := 1)


(defn epoch-week-from-epoch-day
  "Like epoch-day, epoch-week is an ever increasing
  week number, starting with 0 at the week of Monday, 30.12.1969.

  epoch-week-zero-start-day = 30.12.1969  epoch-day=0 6.1.1970
                                       |  1.1.1970    |
                                       |    |         MONDAY
                                       |    |         |
  EPOCH-DAY  -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
  EPOCH-WEEK  -2 -1 -1 -1 -1 -1 -1 -1  0  0 0 0 0 0 0 1 1 1
  "
  [epoch-day]
  (let [epoch-day (+ epoch-day 2)
        epoch-day (if (neg? epoch-day)
                    (- epoch-day 6)
                    epoch-day)]
    (quot epoch-day 7)))

(defn epoch-week
  "Delivers an absolute epoch-week number
  that is linear over all years.
  Starting with 0 at the week of Monday, 30.12.1969.
  So the epoch week with the number 0 contains the
  epoch-day with number 0: 1.1.1970.
  The numbers are negative before.

  epoch-week-zero-start-day = 30.12.1969  epoch-day=0 6.1.1970
                                       |  1.1.1970    |
                                       |    |         MONDAY
                                       |    |         |
  EPOCH-DAY  -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
  EPOCH-WEEK  -2 -1 -1 -1 -1 -1 -1 -1  0  0 0 0 0 0 0 1 1 1
  "
  [tick-date]
  (let [epoch-day  (.toEpochDay tick-date)
        epoch-week (epoch-week-from-epoch-day epoch-day)]
    epoch-week))

(def epoch-week-zero-start-day (d "1969-12-30"))

(tests
  ;(bd/date-breakdown (d "1970-01-05")) ; => MONDAY

  (let [test-data (->> (range -20 15)
                       (map (fn [val]
                              [val {:epoch-week (epoch-week-from-epoch-day val)
                                    :date       (t/new-date val)}]))
                       (into (sorted-map)))
        ed->ew    (fn [ed] (-> ed test-data :epoch-week))]
    (ed->ew -10) := -2
    (ed->ew -9) := -1 ; --- epoch week -1 ---
    (ed->ew -8) := -1
    (ed->ew -7) := -1
    (ed->ew -6) := -1
    (ed->ew -5) := -1
    (ed->ew -4) := -1
    (ed->ew -3) := -1
    (ed->ew -2) := 0 ; --- epoch week 0 ---
    (ed->ew -1) := 0
    (ed->ew 0) := 0
    (ed->ew 1) := 0
    (ed->ew 2) := 0
    (ed->ew 3) := 0
    (ed->ew 4) := 0
    (ed->ew 5) := 1 ; --- epoch week 1 ---
    (ed->ew 6) := 1
    (ed->ew 7) := 1
    (ed->ew 8) := 1
    (ed->ew 9) := 1
    (ed->ew 10) := 1
    (ed->ew 11) := 1
    (ed->ew 12) := 2 ; --- epoch week 2 ---
    :end)
  :end)


(defn date-breakdown
  "Takes a tick/date and breaks it down into units
  including iso week of year (week-of-week-based-year)
  and the corresponding year (week-based-year).
  In addition, there is an abs-week."

  [tick-date]
  (let [[y m d week-day epoch-day] (ymd-vec tick-date)
        [week-based-year week-of-week-based-year] (iso-week-year tick-date)
        epoch-week (epoch-week tick-date)]
    ;abs-week (get-abs-week tick-date)]

    {:month                   m
     :day                     d
     :year                    y
     :week-based-year         week-based-year
     :week-of-week-based-year week-of-week-based-year
     :day-of-week             week-day
     :epoch-day               epoch-day
     :epoch-week              epoch-week}))
;:abs-week                abs-week}))

(tests
  (date-breakdown (t/date "2027-01-03")) := {:month                   1,
                                             :day                     3,
                                             :year                    2027,
                                             :week-based-year         2026,
                                             :week-of-week-based-year 53
                                             :day-of-week             :SUNDAY
                                             :epoch-day               20821
                                             :epoch-week              2974})
;:abs-week                887})

(defn get-date-data
  "[1082 2030 39 203039]"
  [tick-date]
  (let [{:keys [epoch-week
                week-based-year
                week-of-week-based-year]} (date-breakdown tick-date)]
    [epoch-week
     week-based-year
     week-of-week-based-year]))

(tests
  (get-date-data (t/date "1970-01-01")) := [0 1970 1]
  (get-date-data (t/date "2030-09-27")) := [3169 2030 39]
  (get-date-data (t/date "2010-01-04")) := [2087 2010 1]
  (get-date-data (t/date "2039-12-31")) := [3652 2039 52]
  ;(expect-ex (get-abs-week-old (t/date "2040-01-01"))) := #?(:clj AssertionError :cljs js/Error)
  ;(expect-ex (get-abs-week-old (t/date "2010-01-03"))) := #?(:clj AssertionError :cljs js/Error)
  nil)


(defn weekify-element
  "An element is a map that contains key k, e.g. :k as LocalDate.
  Returns the map with an additional key:
  :k-cw will be there representing the week
  as abs-week as [abs-week year-of-week-year week]"
  ; TODO: change to get-abs-week NEW
  [element k]
  (assert (k element) (str "k needs to be a valid key of element, k: " k ", element: " element))
  (assert (or (t/date? (k element))
              (vector? (k element))
              (list? (k element)))
          (str "element needs to be a date or a vector or list of dates; element: " element))

  (let [week-k (keyword (clojure.string/join (into (vec (drop 1 (str k))) (vec "-cw"))))]
    (if (t/date? (k element))
      (assoc element
        week-k (get-date-data (k element)))
      (assoc element
        week-k (map #(get-date-data %) (k element))))))

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

(defn get-first-epoch-day-from-epoch-week
  "Get the first epoch-day of an epoch-week,
  e.g.
  2 --> 12
  -1 --> 9
  epoch-week-zero-start-day = 30.12.1969  epoch-day=0 6.1.1970
                                       |  1.1.1970    |
                                       |    |         MONDAY
                                       |    |         |
  EPOCH-DAY  -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  EPOCH-WEEK  -2 -1 -1 -1 -1 -1 -1 -1  0  0 0 0 0 0 0 1 1 1 1 1  1  1  2  2
  "
  [epoch-week]
  (let [epoch-week (* 7 epoch-week)
        epoch-week (- epoch-week 2)]
    epoch-week)
  #_(let [epoch-day (+ epoch-day 2)
          epoch-day (if (neg? epoch-day)
                      (- epoch-day 6)
                      epoch-day)]
      (quot epoch-day 7)))

(tests
  (def fed get-first-epoch-day-from-epoch-week)
  (fed 2) := 12
  (fed 1) := 5
  (fed 0) := -2
  (fed -1) := -9
  (fed -2) := -16)

(defn week-year-from-abs-week
  "delivers [1970 1 197001] from 1"
  [epoch-week]
  (let [epoch-day (get-first-epoch-day-from-epoch-week epoch-week)
        date      (t/new-date epoch-day)
        {:keys [week-of-week-based-year week-based-year]}
        (date-breakdown date)]
    [week-based-year week-of-week-based-year (+ (* 100 week-based-year) week-of-week-based-year)]))


(defn weeks-from-abs-weeks [start-week num-weeks]
  (vec (map (fn [e] (week-year-from-abs-week (+ start-week e 1)))
            (range num-weeks))))

(defn weeks-indicators [all-year-weeks]
  (map #(str (first %) "-" (second %)) all-year-weeks))

(tests
  (weeks-from-abs-weeks 0 3) := [[1970 2 197002] [1970 3 197003] [1970 4 197004]] #_[[2010 1 201001] [2010 2 201002] [2010 3 201003]]
  (weeks-from-abs-weeks 701 1) := [[1983 24 198324]]
  (weeks-indicators (weeks-from-abs-weeks -2 3)) := '("1969-52" "1970-1" "1970-2")
  nil)
