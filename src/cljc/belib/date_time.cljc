(ns belib.date-time
  "
   ISO calendar weeks
   in cljs and clj...
   https://en.wikipedia.org/wiki/ISO_week_date
   Needs tick:
   https://juxt.github.io/tick/

   --------------------------

   CONCEPTS:

   1. Helper functions for iso weeks calculations,
   like eg:

   (date-breakdown (t/date \"2027-01-03\"))
   =>
   {:month                   1,
    :day                     3,
    :year                    2027,
    :week-based-year         2026,    <---
    :week-of-week-based-year 53       <---
    :day-of-week             :SUNDAY
    :epoch-day               20821
    :epoch-week              2974}

   (monday-from-year-week [1970 1])
   => 1969-12-29)

   2. Analogous to epoch-day, an increasing
      calendar week called epoch-week.
      See function epoch-week-from-epoch-day
      for details.
      Epoch week 0 starts on Monday, 30.12.1969.
      So Epoch week 0 contains epoch day 0: 1.1.1970

        (epoch-week (t/date \"2023-12-31\"))
        ;=> 2817

        (cal-weeks-from-epoch-weeks 0 3)
        ;=> [[1970 2 197002] [1970 3 197003] [1970 4 197004]]

        (weeks-indicators (cal-weeks-from-epoch-weeks -2 3))
        ;=> '(\"1969-52\" \"1970-1\" \"1970-2\")


   3. weekify
      Every date in a map, e.g. :birthday in the map
      (def m {:name :Benno :birthday #time/date 1969-07-14})
      can be weekified, that means, there will be a key:
      :birthday-cw with week data in the map, namely
      [epoch-week week-based-year week-of-week-based-year], e.g.:

      (weekify {:name :Benno
                :birthday (d \"2034-12-01\")}
               :birthday)
      => {:name :Benno
          :birthday #time/date\"2034-12-01\",
          :birthday-cw [3387 2034 48]}

      (update-date-in [{} {:benno {:birthday (d \"2023-01-01\")}}] ;data
                      [1 :benno] ;path to 'entity'
                      :birthday ;key of date
                      t/>> 7) ;fn and parameters
      => [{} {:benno {:birthday #time/date\"2023-01-08\"
                      :birthday-cw [2766 2023 1]}}]

  "
  (:require
    ;[clojure.test :as t]
    [cuerdas.core :as str]
    [cljc.java-time.local-date :as local-date]
    [tick.core :as t]
    [cljc.java-time.temporal.chrono-unit :as cu]
    [hyperfiddle.rcf :refer [tests]]
    [tick.alpha.interval :as tai]
    [belib.core :as bc]
    #_#?(:cljs [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break
                                                   clog_ clogn_ dbg_ dbgn_ break_]]
         :clj  [debux.cs.core :as d :refer [clog clogn dbg dbgn break
                                            clog_ clogn_ dbg_ dbgn_ break_]])
    #?@(:cljs [[goog.string :as gstring]
               [java.time :refer [LocalDateTime LocalDate Year]]

               [belib.test :refer-macros [expect-ex return-ex return-error-kw-if-ex]]])
    #?@(:clj [[belib.test :refer [expect-ex return-ex return-error-kw-if-ex]]
              [java-time.api :as jt]]))
  #?(:clj
     (:import [java.time ZoneOffset LocalDateTime Year LocalDate ZonedDateTime]
              [java.time.temporal IsoFields]
              [java.util Date])))


(hyperfiddle.rcf/enable! true)

(comment
  (require 'playback.core) ; open the portal
  (use '[debux.core])
  (dbgn (+ 3 (- 4 5))))


(def d
  "create a date, e.g (d \"2003-01-01\")"
  t/date)

(def dt
  "create a date-time, e.g. (dt \"2003-01-01T00:00\")"
  t/date-time)

(tests
  "types of dates"
  (type (dt "2003-01-01T00:00")) := LocalDateTime ; in cls and cljs!
  (instance? LocalDateTime (dt "2003-01-01T00:00")) := true
  (type (d "2023-01-05")) := LocalDate ; in cls and cljs!

  :end-test)

(tests
  "parsing and predicates for dates"
  (return-error-kw-if-ex (d "2003-1-1")) := :error
  (t/date? (d "2003-01-01")) := true
  (t/date? (dt "2003-01-01T00:00")) := false
  (t/date-time? (dt "2003-01-01T00:00")) := true
  (t/date-time? (d "2003-01-01")) := false

  :end-test)

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
  (ymd-hms-vec (t/date-time "2027-01-03T21:23:12")) := [2027 1 3 21 23 12]
  (t/year (t/date-time "2027-01-03T21:23:12")) := #?(:cljs (Year. 2027) :clj (Year/of 2027))

  :end-test)

(defn ymd-vec
  "Vector [year month day weekday epoch-day] of a tick date."
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
  (date-to-str (t/date "2027-01-03")) := "2027-01-03"

  :end-test)

(defn date-time-to-str
  "do simple formatting - also cljs - without having
  required [tick.locale-en-us]"
  [tick-date-time]
  (let [[y m d h mi s] (ymd-hms-vec tick-date-time)]
    (#?(:cljs gstring/format
        :clj  format)
      "%4d-%02d-%02d %02d:%02d:%02d" y m d h mi s)))

(tests
  (date-time-to-str (t/date-time "2027-01-03T21:22:23")) := "2027-01-03 21:22:23"

  :end-test)


(defn tick-date-to-native-date
  "convert tick-date (LocalDate (jvm) or joda Date (cljs))
   to either java.util.Date (clj) or js/Date (cljs)"
  [tick-date]
  #?(:cljs
     (let [year  (t/int (t/year tick-date))
           month (t/int (t/month tick-date))
           day   (t/day-of-month tick-date)]
       (js/Date. (js/Date.UTC year (dec month) day 0 0 0 0)))
     :clj
     (let [[y m d] (ymd-vec tick-date)
           ldt (LocalDateTime/of y m d 0 0)
           zdt (.atZone ldt ZoneOffset/UTC)]
       (Date/from (.toInstant zdt)))))

(comment
  (type (tick-date-to-native-date (t/date)))

  ; native-date-to-tick-date? No...
  (type (t/date-time (java.util.Date.))) ; this is local
  (type (t/date (java.util.Date.)))

  ; same for cljs
  (type (t/date-time (js/Date.)))
  (type (t/date (js/Date.)))

  :end)

(tests
  (some? (tick-date-to-native-date (t/date "2027-01-03"))) := true
  (tick-date-to-native-date (t/date "2027-01-03")) := #inst"2027-01-03T00:00:00Z"
  #?(:clj
     (tests (type (tick-date-to-native-date (t/date))) := java.util.Date)
     :cljs
     (tests (type (tick-date-to-native-date (t/date))) := js/Date))

  :end-test)


(defn list-of-all-days
  "Range of all days starting with start
  excluding end."
  [start end]
  (map t/date
       (t/range
         start
         end
         (t/new-period 1 :days))))

(tests
  (count (list-of-all-days
           (dt "2027-01-01T00:00")
           (dt "2027-01-05T00:00"))) := 4

  :end-test)

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

(comment
  (t/days (t/duration (tai/new-interval (d "2000-01-01") (d "2000-12-31"))))
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
   (assert (t/date-time? start) (str "start = " start ", but start needs to be date-time, create with (dt \"2023-01-01T00:01\")"))
   (assert (t/date-time? end) (str "end = " end ", but end needs to be date-time, create with (dt \"2023-01-01T00:01\")"))
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

#?(:cljs ; impl for iso-week-year
   (defn- week-of-week-based-year [tick-date]
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

#?(:cljs ; impl for iso-week-year
   (defn- week-based-year [tick-date]
     (let [d (tick-date-to-native-date tick-date)]
       (.setDate d (+ (.getDate d) 3 (- (mod (+ (.getDay d) 6) 7))))
       (.getFullYear d))))

(defn iso-week-year
  "returns iso-week data:
  (iso-week-year (t/date \"2027-01-03\"))
  ;=> [2026 53]"
  [tick-date]
  #?(:cljs
     [(week-based-year tick-date) (week-of-week-based-year tick-date)]
     :clj
     [(.get tick-date #_(LocalDate/of 2023 1 1) IsoFields/WEEK_BASED_YEAR)
      (.get tick-date #_(LocalDate/of 2023 1 1) IsoFields/WEEK_OF_WEEK_BASED_YEAR)]
     #_(let [all-data (jt/as-map tick-date)]
         [(:week-based-year all-data)
          (:week-of-week-based-year all-data)])))

(tests
  (iso-week-year (t/date (t/instant "2014-12-31T00:00:00Z"))) := [2015 1]
  (iso-week-year (t/date "1970-01-01")) := [1970 1]
  (iso-week-year (t/date "2017-01-01")) := [2016 52]
  (iso-week-year (t/date "2018-12-31")) := [2019 1]
  (iso-week-year (t/date "2019-01-01")) := [2019 1]
  (iso-week-year (t/date "2026-12-27")) := [2026 52]
  (iso-week-year (t/date "2027-01-03")) := [2026 53]
  (iso-week-year (t/date "2027-01-04")) := [2027 1]
  :end-tests)


#_(def first-day-of-abs-week-0
    "the definition of the origin of abs-week,
  2009-12-28, put as epoch-day: 14606"
    (.toEpochDay (d "2009-12-28")))

#_(defn get-abs-week
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

#_(tests
    (get-abs-week (d "2009-12-21")) := -1
    (get-abs-week (d "2009-12-28")) := 0
    (get-abs-week (d "2010-01-04")) := 1)


(defn epoch-week-from-epoch-day
  "Like epoch-day, epoch-week is an ever increasing
  week number, starting with 0 at the week of Monday, 30.12.1969.

   epoch-week-zero-start-day = 30.12.1969  epoch-day=0 5.1.1970
                                     |     1.1.1970    |
                                     |       |        / MONDAY
                                     |       |       |
  EPOCH-DAY   -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
  EPOCH-WEEK   -1 -1 -1 -1 -1 -1 -1  0  0  0 0 0 0 0 1 1 1
  "
  [epoch-day]
  (let [epoch-day (+ epoch-day 3)
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
  "
  [tick-date]
  (let [epoch-day  (.toEpochDay tick-date)
        epoch-week (epoch-week-from-epoch-day epoch-day)]
    epoch-week))


(tests

  (let [test-data (->> (range -20 15)
                       (map (fn [val]
                              [val {:epoch-week (epoch-week-from-epoch-day val)
                                    :date       (t/new-date val)}]))
                       (into (sorted-map)))
        ed->ew    (fn [ed] (-> ed test-data :epoch-week))]
    (ed->ew -11) := -2
    (ed->ew -10) := -1 ; --- epoch week -1 ---
    (ed->ew -9) := -1
    (ed->ew -8) := -1
    (ed->ew -7) := -1
    (ed->ew -6) := -1
    (ed->ew -5) := -1
    (ed->ew -4) := -1
    (ed->ew -3) := 0 ; --- epoch week 0 ---
    (ed->ew -2) := 0
    (ed->ew -1) := 0
    (ed->ew 0) := 0
    (ed->ew 1) := 0
    (ed->ew 2) := 0
    (ed->ew 3) := 0
    (ed->ew 4) := 1 ; --- epoch week 1 ---
    (ed->ew 5) := 1
    (ed->ew 6) := 1
    (ed->ew 7) := 1
    (ed->ew 8) := 1
    (ed->ew 9) := 1
    (ed->ew 10) := 1
    (ed->ew 11) := 2 ; --- epoch week 2 ---
    (ed->ew 12) := 2
    :end)
  :end)


(defn date-breakdown
  "Takes a tick/date and breaks it down into units
  including iso week of year (week-of-week-based-year)
  and the corresponding year (week-based-year).
  In addition, there is an epoch-week, that starts with 0
  when epoch-day starts with 0. Example:
  (date-breakdown (t/date \"2027-01-03\"))
  result:
  {:month                   1,
   :day                     3,
   :year                    2027,
   :week-based-year         2026,
   :week-of-week-based-year 53
   :day-of-week             :SUNDAY
   :epoch-day               20821
   :epoch-week              2974}"

  [tick-date]
  (let [[y m d week-day epoch-day] (ymd-vec tick-date)
        [week-based-year week-of-week-based-year] (iso-week-year tick-date)
        epoch-week (epoch-week tick-date)]
    {:month                   m
     :day                     d
     :year                    y
     :week-based-year         week-based-year
     :week-of-week-based-year week-of-week-based-year
     :day-of-week             week-day
     :epoch-day               epoch-day
     :epoch-week              epoch-week}))

(tests
  (date-breakdown (t/date "2027-01-03")) := {:month                   1,
                                             :day                     3,
                                             :year                    2027,
                                             :week-based-year         2026,
                                             :week-of-week-based-year 53
                                             :day-of-week             :SUNDAY
                                             :epoch-day               20821
                                             :epoch-week              2974}
  (date-breakdown (t/date "2023-09-24")) := {:month                   9,
                                             :day                     24,
                                             :year                    2023,
                                             :week-based-year         2023,
                                             :week-of-week-based-year 38
                                             :day-of-week             :SUNDAY
                                             :epoch-day               19624
                                             :epoch-week              2803}
  (date-breakdown (t/date "1970-01-01")) := {:month                   1,
                                             :day                     1,
                                             :year                    1970,
                                             :week-based-year         1970,
                                             :week-of-week-based-year 1
                                             :day-of-week             :THURSDAY
                                             :epoch-day               0
                                             :epoch-week              0}

  :end-tests)


(def epoch-week-zero-start-day
  "First day of epoch week 0."
  (d "1969-12-29"))

(tests
  (-> epoch-week-zero-start-day (t/<< 1) date-breakdown :epoch-week) := -1
  (-> epoch-week-zero-start-day date-breakdown :epoch-week) := 0) ; => first day of week 0

(defn epoch+iso-week-data
  "(epoch+iso-week-data (t/date \"2018-12-31\"))
  returns:
  [2556 2019 1] epoch-week, week-based-year, week-of-week-based-year.
  ATTENTION: While the date is in 2018, the week-based-year is 2019."
  [tick-date]
  (let [{:keys [epoch-week
                week-based-year
                week-of-week-based-year]} (date-breakdown tick-date)]
    [epoch-week
     week-based-year
     week-of-week-based-year]))

(tests
  (epoch+iso-week-data (t/date "1970-01-01")) := [0 1970 1]
  (epoch+iso-week-data (t/date "2018-12-31")) := [2557 2019 1]
  (epoch+iso-week-data (t/date "2010-01-04")) := [2088 2010 1]
  (epoch+iso-week-data (t/date "2039-12-31")) := [3652 2039 52]
  :end-tests)


(defn weekify
  "An element is a map that contains key :k with a value that is a tick-date.
  Returns the map with an additional key:
  :k-cw will be there representing the week
  as [epoch-week year-of-week-year week-of-year]"
  [element k]
  (assert (k element) (str "k needs to be a valid key of element, k: " k ", element: " element))
  (assert (or (t/date? (k element))
              (vector? (k element))
              (list? (k element)))
          (str "element needs to be a date or a vector or list of dates; element: " element))
  (let [week-k (keyword (clojure.string/join (into (vec (drop 1 (str k))) (vec "-cw"))))]
    (if (t/date? (k element))
      (assoc element
        week-k (epoch+iso-week-data (k element)))
      (assoc element
        week-k (map #(epoch+iso-week-data %) (k element))))))

(tests
  (weekify {:birthday (d "2034-12-01")} :birthday)
  := {:birthday (t/date "2034-12-01"), :birthday-cw [3387 2034 48]}

  :end-tests)

(defn weekify-in
  [m ks date-key]
  (let [;_           (println "m: " m ", ks: " ks)
        sub-m       (get-in m ks)
        weekified   (weekify sub-m date-key)
        m-weekified (assoc-in m ks weekified)]
    m-weekified))

(tests
  (weekify-in [{} {:benno {:birthday (d "2034-12-01")}}]
              [1 :benno]
              :birthday)
  := [{} {:benno {:birthday (t/date "2034-12-01"), :birthday-cw [3387 2034 48]}}]

  :end-tests)


(comment
  (get-in [{} {:benno {:birthday (d "2034-12-01")}}]
          [1 :benno])
  ;(dt (d "2027-01-03"))
  (bc/bpp (date-breakdown (t/date "2027-01-03")))
  (bc/bpp (date-breakdown (t/date "2027-01-03T00:00:00")))
  (bc/bpp (list-of-all-days (dt "2027-01-01T00:00") (dt "2027-01-05T00:00")))

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
  epoch-week-zero-start-day = 29.12.1969     epoch-day=0   6.1.1970
                                        |     1.1.1970    /
                                        |       |        / MONDAY
                                        |       |       |
  EPOCH-DAY  -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  EPOCH-WEEK  -2  -1 -1 -1 -1 -1 -1 -1  0  0  0 0 0 0 0 1 1 1 1 1 1  1  2  2
  "
  [epoch-week]
  (let [epoch-week (* 7 epoch-week)
        epoch-week (- epoch-week 3)]
    epoch-week)
  #_(let [epoch-day (+ epoch-day 2)
          epoch-day (if (neg? epoch-day)
                      (- epoch-day 6)
                      epoch-day)]
      (quot epoch-day 7)))

(tests
  (def fed get-first-epoch-day-from-epoch-week)
  (fed 2) := 11
  (fed 1) := 4
  (fed 0) := -3
  (fed -1) := -10
  (fed -2) := -17

  :end-tests)

(defn week-year-from-epoch-week
  "returns: [1970 1]
  from parameter:
  1"
  [epoch-week]
  (let [epoch-day (get-first-epoch-day-from-epoch-week epoch-week)
        date      (t/new-date epoch-day)
        {:keys [week-of-week-based-year week-based-year]}
        (date-breakdown date)]
    [week-based-year week-of-week-based-year]))


(defn weeks-col-from-epoch-weeks
  "parameters: start-week = 0
               num-weeks = 3
   returns:  [[1970 2]
             [1970 3]
             [1970 4]]
  "
  [start-week num-weeks]
  (mapv (fn [e] (week-year-from-epoch-week (+ start-week e 1)))
        (range num-weeks)))

(defn weeks-indicators
  "this call:
  (weeks-indicators (weeks-col-from-epoch-weeks -2 3))
  returns this:
  (\"1969-52\" \"1970-1\" \"1970-2\")"
  [all-year-weeks]
  (map #(str (first %) "-" (second %)) all-year-weeks))

(tests
  (weeks-col-from-epoch-weeks 0 3) := [[1970 2 #_197002] [1970 3 #_197003] [1970 4 #_197004]] #_[[2010 1 201001] [2010 2 201002] [2010 3 201003]]
  (weeks-col-from-epoch-weeks 701 1) := [[1983 24 #_198324]]
  (weeks-indicators (weeks-col-from-epoch-weeks -2 3)) := '("1969-52" "1970-1" "1970-2")

  :end-tests)


(defn epoch-day [tick-date]
  (.toEpochDay tick-date))

(tests
  (epoch-week (t/date "1970-01-01")) := 0
  (epoch-day (t/date "1970-01-01")) := 0
  :end-tests)

(defn rand-date-between [from-date to-date]
  (let [start-epoch-day (epoch-day from-date)
        end-epoch-day   (epoch-day to-date)
        range           (- end-epoch-day start-epoch-day)]
    (t/new-date (+ start-epoch-day (rand-int range)))))

(tests
  (-> (repeatedly 1000 #(rand-date-between (d "2023-01-01") (d "2023-01-10")))
      (frequencies)
      (keys)
      (count)) := 9)

(defn assoc-date-in
  "when assoc-in a date, weekify the date."
  [m ks date-key date]
  (let [m-date      (assoc-in m (conj ks date-key) date)
        weekified   (weekify (get-in m-date ks) date-key)
        m-weekified (assoc-in m-date ks weekified)]
    m-weekified))

(defn update-date-in
  "when update-in a date, weekify the date."
  [m ks date-key date-fn & args]
  (let [m-date      (update-in m
                               (conj ks date-key)
                               #(apply date-fn (cons % args)))
        weekified   (weekify (get-in m-date ks)
                             date-key)
        m-weekified (assoc-in m-date ks weekified)]
    m-weekified))



(tests
  (assoc-date-in [{} {:benno {:birthday (d "2023-01-01")}}]
                 [1 :benno]
                 :birthday
                 (d "1999-12-31"))
  := [{} {:benno {:birthday    (d "1999-12-31"),
                  :birthday-cw [1565 1999 52]}}]

  (t/>> (d "2023-01-01") 5) := (d "2023-01-06")

  "update-in-date with a function with one parameters"
  (update-date-in [{} {:benno {:birthday (d "2023-01-01")}}]
                  [1 :benno]
                  :birthday
                  t/>> 7)
  := [{} {:benno {:birthday    (d "2023-01-08"),
                  :birthday-cw [2766 2023 1]}}]


  "update-in-date with a function with additional parameters"
  (-> (update-date-in [{} {:benno {:birthday (d "2023-01-01")}}]
                      [1 :benno]
                      :birthday

                      rand-date-between (d "2229-01-05")) ; faaaaaaaaar in future
      (get-in [1 :benno :birthday-cw])
      first
      get-first-epoch-day-from-epoch-week
      t/new-date
      (t/> (d "2023-01-01")))
  := true
  :end-tests)

(defn year-week-long-to-data [year-week-long]
  (let [y (quot year-week-long 100)
        w (mod year-week-long 100)]
    (assert (and (> w 0) (< w 54)))
    (assert (and (> y 0) (< y 5000)))
    [y w]))

(tests

  "long year week works"
  (year-week-long-to-data 202303) := [2023 3]

  "year too big"
  (expect-ex (year-week-long-to-data 500003))
  := #?(:clj AssertionError :cljs js/Error)

  "negative"
  (expect-ex (year-week-long-to-data -202303))
  := #?(:clj AssertionError :cljs js/Error)

  :end-test)


(defn year-week-str-to-data [year-week-str]
  (let [sep   (subs year-week-str 5 6)
        shift (if (or (= sep "W") (= sep "w")) 1 0)
        ys    (subs year-week-str 0 4)
        y     (bc/parse-long ys)
        ws    (subs year-week-str (+ 5 shift) (count year-week-str))
        w     (bc/parse-long ws)]
    [y w]))

(tests
  (year-week-str-to-data "2023-4") := [2023 4]
  (year-week-str-to-data "2023-04") := [2023 4]
  (year-week-str-to-data "2023-w04") := [2023 4]
  (year-week-str-to-data "2023-11") := [2023 11]
  (year-week-str-to-data "2023-W11") := [2023 11]

  :end-tests)

(defn monday
  "Monday before the date - or the date itself, if monday."
  [date]
  (let [diff   (dec (t/int (t/day-of-week date)))
        monday (t/<< date diff)]
    monday))

(defn monday-from-year-week
  "get the monday of a given year-week.
  E.g [1970 1] => 1969-12-29"
  [[y w]]
  (let [days         (-> w (- 1) (* 7) (+ 4))
        dummy-date   (t/new-date y 1 1)
        days-in-year (local-date/length-of-year dummy-date)
        [y days] (if (> days days-in-year) [(inc y) (- days days-in-year)]
                                           [y days])
        date         (t/new-date y days)]
    (monday date)))


(tests

  "first iso calendar week 1970 starts 1969"
  (monday-from-year-week [1970 1]) := (d "1969-12-29")


  "test, if calcing mondays from [year week] works especially at the borders between the years."
  (let [all-days  (list-of-all-days (d "1930-01-01") (d "2133-01-01"))
        test-data (map (fn [d]
                         ;(println d)
                         (let [bd        (date-breakdown d)
                               bd-monday (monday d)
                               yw-monday (monday-from-year-week [(:week-based-year bd)
                                                                 (:week-of-week-based-year bd)])]
                           {:breakdown        bd
                            :breakdown-monday bd-monday
                            :year-week-monday yw-monday}))
                       all-days)]
    (reduce (fn [acc {:keys [breakdown-monday year-week-monday]}]
              (assert (some? breakdown-monday))
              (assert (some? year-week-monday))
              (assert (= breakdown-monday year-week-monday))
              (inc acc))
            0
            test-data))
  := 74145 ; tested that many days from 1930 to 2133

  :end-test)

;; extend some protocols for simpler usage

(defprotocol YearWeekLocalDatable
  (to-d [this]))

(extend-protocol YearWeekLocalDatable

  #?(:cljs string :clj java.lang.String)
  (to-d [this] (monday-from-year-week (year-week-str-to-data this)))

  #?(:cljs number :clj java.lang.Long)
  (to-d [this] (monday-from-year-week (year-week-long-to-data this))))

(tests
  (to-d "2023-w01") := (d "2023-01-02")
  (to-d 202301) := (d "2023-01-02")
  :end-tests)



(defprotocol ConvertTo
  (to-date [this])
  (to-epoch-day [this])
  (to-epoch-week [this])
  (to-week-year [this])
  (to-str [this])
  (to-long [this])
  (to-data [this]))

(defrecord EpochWeek [epoch-week])
(defrecord YearWeek [year week])

(extend-protocol ConvertTo

  EpochWeek
  (to-date [this] (-> (:epoch-week this) get-first-epoch-day-from-epoch-week t/new-date monday))
  (to-epoch-day [this] (-> (:epoch-week this) get-first-epoch-day-from-epoch-week))
  (to-epoch-week [this] this)
  (to-week-year [this] (-> (:epoch-week this) week-year-from-epoch-week))
  (to-str [this] (str "epoch-week " (:epoch-week this)))
  (to-long [this] (:epoch-week this))
  (to-data [this] [(:epoch-week this)])

  YearWeek
  (to-date [this] (monday-from-year-week [(:year this) (:week this)]))
  (to-epoch-day [this] (-> [(:year this) (:week this)] monday-from-year-week epoch-day))
  (to-epoch-week [this] (-> [(:year this) (:week this)] monday-from-year-week epoch-week ->EpochWeek))
  (to-week-year [this] this)
  (to-str [this] (str (:year this) "-W" (:week this)))
  (to-long [this] (-> 100 (* (:year this)) (+ (:week this))))
  (to-data [this] [(:year this) (:week this)]))


(tests

  "everything about converting EpochWeek"
  (let [ew (->EpochWeek 0)]
    (to-date ew) := (d "1969-12-29")
    (to-epoch-day ew) := -3
    (to-epoch-week ew) := (EpochWeek. 0) ; same as (->EpochWeek 0)
    (to-week-year ew) := [1970 1]
    (to-str ew) := "epoch-week 0"
    (to-long ew) := 0
    (to-data ew) := [0])

  "everything about converting YearWeek"
  (let [yw (->YearWeek 1970 1)]
    (to-date yw) := (d "1969-12-29")
    (to-epoch-day yw) := -3
    (to-epoch-week yw) := (EpochWeek. 0) ; same as (->EpochWeek 0)
    (to-week-year yw) := yw ; NOT [1970 1]
    (to-str yw) := "1970-W1"
    (to-long yw) := 197001
    (to-data yw) := [1970 1])

  :end-test)



(defprotocol ToYearWeekI
  "Everything that may be converted to a year week - by (to-yw ...)"
  (to-yw [this]))

(extend-protocol ToYearWeekI

  LocalDate
  (to-yw [this] (epoch+iso-week-data this))

  ; STRING "2016-W23"
  #?(:cljs string :clj java.lang.String)
  (to-yw [this] (epoch+iso-week-data (monday-from-year-week (year-week-str-to-data this))))

  ; LONG 201623
  #?(:cljs number :clj java.lang.Long)
  (to-yw [this] (epoch+iso-week-data (monday-from-year-week (year-week-long-to-data this)))))

(tests

  (to-yw (d "1969-12-21")) := [-2 1969 51]
  (to-yw (d "1969-12-22")) := [-1 1969 52]
  (to-yw (d "1969-12-23")) := [-1 1969 52]
  (to-yw (d "1969-12-24")) := [-1 1969 52]
  (to-yw (d "1969-12-25")) := [-1 1969 52]
  (to-yw (d "1969-12-26")) := [-1 1969 52]
  (to-yw (d "1969-12-27")) := [-1 1969 52]
  (to-yw (d "1969-12-28")) := [-1 1969 52]
  (to-yw (d "1969-12-29")) := [0 1970 1] ; <- if you choose (to-yw 197001) you end up here
  (to-yw (d "1969-12-30")) := [0 1970 1]
  (to-yw (d "1969-12-31")) := [0 1970 1]
  (to-yw (d "1970-01-01")) := [0 1970 1] ; <- if you choose (to-yw (d "1970-01-01")) you end up here
  (to-yw (d "1970-01-02")) := [0 1970 1]
  (to-yw (d "1970-01-03")) := [0 1970 1]
  (to-yw (d "1970-01-04")) := [0 1970 1]
  (to-yw (d "1970-01-05")) := [1 1970 2]
  (to-yw (d "1970-01-06")) := [1 1970 2]

  (to-yw (d "1970-01-01")) := [0 1970 1]
  (to-yw "1970-W01") := [0 1970 1]
  (to-yw 197001) := [0 1970 1]

  "make sure, mondays, year-week, epoch-week and epoch day really match."
  (mapv (fn [d] [(date-to-str d)
                 (to-yw d)
                 (epoch-day d)
                 (:day-of-week (date-breakdown d))])
        (list-of-all-days (d "1969-12-10") (d "1970-01-10")))
  := [["1969-12-10" [-3 1969 50] -22 :WEDNESDAY]
      ["1969-12-11" [-3 1969 50] -21 :THURSDAY]
      ["1969-12-12" [-3 1969 50] -20 :FRIDAY]
      ["1969-12-13" [-3 1969 50] -19 :SATURDAY]
      ["1969-12-14" [-3 1969 50] -18 :SUNDAY]

      ["1969-12-15" [-2 1969 51] -17 :MONDAY]
      ["1969-12-16" [-2 1969 51] -16 :TUESDAY]
      ["1969-12-17" [-2 1969 51] -15 :WEDNESDAY]
      ["1969-12-18" [-2 1969 51] -14 :THURSDAY]
      ["1969-12-19" [-2 1969 51] -13 :FRIDAY]
      ["1969-12-20" [-2 1969 51] -12 :SATURDAY]
      ["1969-12-21" [-2 1969 51] -11 :SUNDAY]

      ["1969-12-22" [-1 1969 52] -10 :MONDAY]
      ["1969-12-23" [-1 1969 52] -9 :TUESDAY]
      ["1969-12-24" [-1 1969 52] -8 :WEDNESDAY]
      ["1969-12-25" [-1 1969 52] -7 :THURSDAY]
      ["1969-12-26" [-1 1969 52] -6 :FRIDAY]
      ["1969-12-27" [-1 1969 52] -5 :SATURDAY]
      ["1969-12-28" [-1 1969 52] -4 :SUNDAY]

      ["1969-12-29" [0 1970 1] -3 :MONDAY] ; start epoch-week 0
      ["1969-12-30" [0 1970 1] -2 :TUESDAY]
      ["1969-12-31" [0 1970 1] -1 :WEDNESDAY]
      ["1970-01-01" [0 1970 1] 0 :THURSDAY] ; epoch-day 0
      ["1970-01-02" [0 1970 1] 1 :FRIDAY]
      ["1970-01-03" [0 1970 1] 2 :SATURDAY]
      ["1970-01-04" [0 1970 1] 3 :SUNDAY]

      ["1970-01-05" [1 1970 2] 4 :MONDAY] ; start epoch week 1 = epoch day 4 = 1970-01-05
      ["1970-01-06" [1 1970 2] 5 :TUESDAY]
      ["1970-01-07" [1 1970 2] 6 :WEDNESDAY]
      ["1970-01-08" [1 1970 2] 7 :THURSDAY]
      ["1970-01-09" [1 1970 2] 8 :FRIDAY]]


  :tests-end)