(ns belib.cal-week-year
  ; TODO: what about the start date of the week in the data of the mappings?
  "
   Handle ISO calendar weeks!
   --------------------------

   CONCEPTS:

   1. mapped to absolute, linear calendar weeks: abs-week
   2. starting from 2010-01-04, abs-week 1
   3. ending with 2039-12-31, abs-week 1565
   4. calendar weeks may be expressed as
      abs-week, starting from 1 ending 1565
      year-week-long: year 2032, week 51 => 201251
   5. Mappings
      A LocalDate -> abs-week (long) and additional calendar week data
        2011-01-10 -> [54 2011 2 201102]
        7 days each result in the same week data!
      B abs-week (long) -> calendar week data
        700 -> [2023 22 202322]
      C year-week-long -> abs-week
        202322 -> [2023 22 202322 700] TODO will change!
   6. Weekify
      Every date in a map, e.g. :birthday in the map
      (def m {:name :Benno :birthday #time/date 1969-07-14})
      can be weekified, that means, there will be a key:
      :birthday-cw with calendar week data in the map.
      (weekify m :birthday)

  API:

   Mappings

   A date -> calender week data
     (get-abs-week (t/date \"2011-01-10\"))
     => [54       2011         2            201102]
        [abs-week year-of-week week-of-year year-week-long]

   B abs-week -> calender week data
     (week-year-from-abs-week 700)
     => [2023 22 202322]

   C year-week-long -> abs-week
     (week-year-from-year-week-long 202322)
     => [2023 22 202322 700] TODO will change!

   Weekify

     (weekify-element
       {:name :Benno :birthday (d \"2020-02-15\")}
       :birthday)

     => {:name :Benno,
         :birthday #time/date\"2020-02-15\",
         :birthday-cw [528 2020 7 202007]}

  "
  (:require [tick.core :as t]
            [hyperfiddle.rcf :refer [tests]]
            [belib.core :as bc]
            [belib.date-time :as bd]
            #?(:clj  [belib.test :as bt :refer [expect-ex return-ex]]
               :cljs [belib.test :as bt :refer-macros [expect-ex return-ex]])
            #?(:cljs [java.time :refer [LocalDateTime LocalDate Instant]])
            #?(:cljs [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break
                                                         clog_ clogn_ dbg_ dbgn_ break_]]
               :clj  [debux.cs.core :as d :refer [clog clogn dbg dbgn break
                                                  clog_ clogn_ dbg_ dbgn_ break_]]))
  #?(:clj (:import [java.time LocalDateTime LocalDate Instant])))

(hyperfiddle.rcf/enable! true)

(def d t/date)

(comment
  (require 'playback.core) ; open the portal
  (dbgn (+ 3 (- 4 5))))
; try in contrast #>>(+ 3 (- 4 5)))

;;--------------------
;; time model of weeks
;;--------------------
;;
;; a linear week model since 2010-01-04 until 2039-12-31 (including).
;; options for handling calender weeks
;;               Date -> [abs-week year week]
;; #inst "2023-04-11" -> [693      2023 15]
;; abs-week starts with 1 at 2010-01-04
;; abs-week ends with 1565 at 2039-12-31

#_(defn date-to-year-week-long-uncached
    "Creates the long 201501,
  representing week 01 in year 2015
  from the tick date 2014-12-31."
    [tick-date]
    (assert (bd/date? tick-date))
    (let [[week-based-year week-of-week-based-year] (bd/iso-week-year tick-date)]
      (-> week-based-year
          (* 100)
          (+ week-of-week-based-year))))

#_(def date-infos-as-long
    "cached version"
    (memoize date-to-year-week-long-uncached))

#_(tests
    (date-infos-as-long (t/date "2014-12-31")) := 201501)

#_(comment
    (map t/date (let [start (t/date-time "2010-01-04T00:00")]
                  (t/range
                    start
                    (t/>> start (t/new-duration (+ (* 365 30) 4) :days))
                    (t/new-period 1 :days)))))


;;
;; Calc the absolute calender weeks data for all days
;;

; first a long list of days
#_(defn- list-of-all-days
    "List all days, beginning with first-day."
    [first-day days]
    (map t/date (let [start (t/date-time first-day)]
                  (t/range
                    start
                    (t/>> start (t/new-duration days :days))
                    (t/new-period 1 :days)))))


#_(def first-day (t/date-time "2010-01-04T00:00"))
#_(def days (+ (* 365 30) 4))
#_(def days-2010-01-04-to-2039-12-31
    "List of :time/date from 2010-01-04 to 2039-12-31, including."
    (list-of-all-days first-day days))

#_(tests
    (take 2 days-2010-01-04-to-2039-12-31) :=
    [(t/date "2010-01-04") (t/date "2010-01-05")]

    (take-last 2 days-2010-01-04-to-2039-12-31) :=
    [(t/date "2039-12-30") (t/date "2039-12-31")])


; then calc it

#_(defn- calc-abs-weeks
    "Starting from a sorted list of consecutive days, each day like this: [year week-of-year date],
  create a list of days like this: [date [year week-of-year absolute-week].
  Absolute week starts with 1 at the beginning and
  increases by one every week."
    [all-weeks]
    (loop [f      (first all-weeks)
           r      (rest all-weeks)
           result (list)]
      (if f
        ; l- last in the sense of 'previous entry'
        ; c- current
        (let [l-week-of-year (get (second (first result)) 2)
              [c-date c-year c-week-of-year] f
              week-jump      (not= l-week-of-year c-week-of-year)
              l-abs-week     (first (second (first result)))
              l-abs-week     (if l-abs-week l-abs-week 0) ; init and start with jump
              c-abs-week     (if week-jump (inc l-abs-week) l-abs-week)]
          (recur (first r)
                 (rest r)
                 (conj result [c-date [c-abs-week
                                       c-year
                                       c-week-of-year
                                       (-> c-year
                                           (* 100)
                                           (+ c-week-of-year))]])))
        result)))


#_(defn- init-absolut-weeks
    "Creates map with
  key = 'local date' and
  value = [absolute-week year week-of-year]"
    [day-list]
    (let [all-days-with-week (map (fn [d] (concat [d] (bd/iso-week-year d)))
                                  day-list)]
      (into
        (sorted-map) ; TODO good idea? just map would do the trick...
        (calc-abs-weeks all-days-with-week))))

#_(tests
    (init-absolut-weeks [(d "2039-12-23")])
    := {(d "2039-12-23") [1 2039 51 203951]})

;;
;; lookup-table for A, date -> abs-week
;;
#_(defonce abs-week-map (init-absolut-weeks days-2010-01-04-to-2039-12-31))
#_(def first-date (first (first abs-week-map)))
#_(def last-date (first (last abs-week-map)))

#_(tests
    (count abs-week-map) := 10954) ; about 11.000 days

#_(defn get-abs-week-old-
    "Delivers an absolute week number that is linear over all years.
  Returns [absolute-week-since-2010-01-04 year week-of-year]
  Starting with 2010-01-04, which is the start of week 1 in that year.
  Ending with 2039-12-31. Before and after that dates, nil results
  will be returned."
    [tick-date]
    (assert (t/date? tick-date))
    (assert (t/>= tick-date first-date) "date has to be after 2010-01-04")
    (assert (t/<= tick-date last-date) "date has to be before 2039-12-31")
    (abs-week-map tick-date))

#_(tests
    (get-abs-week-old- (t/date "2030-09-27")) := [1082 2030 39 203039]
    (get-abs-week-old- (t/date "2010-01-04")) := [1 2010 1 201001]
    (get-abs-week-old- (t/date "2039-12-31")) := [1565 2039 52 203952]
    (expect-ex (get-abs-week-old- (t/date "2040-01-01"))) := #?(:clj AssertionError :cljs js/Error)
    (expect-ex (get-abs-week-old- (t/date "2010-01-03"))) := #?(:clj AssertionError :cljs js/Error)
    nil)


;;
;; lookup-table for B, abs-week -> calender week data
;;

#_(defn init-abs-week-to-year-week-map []
    (reduce (fn [acc [abs-week year-week week year-week-long :as val]]
              (into acc
                    [[abs-week [year-week week year-week-long]]]))
            {}
            (set (vals abs-week-map))))

#_(defonce abs-week-to-year-week-map (init-abs-week-to-year-week-map))

#_(defn week-year-from-abs-week [abs-week]
    (abs-week-to-year-week-map abs-week))

#_(tests
    (week-year-from-abs-week 1) := [2010 1 201001]
    (week-year-from-abs-week 700) := [2023 22 202322])



;;
;; lookup-table for C, year-week-long -> abs-week
;;

#_(defn init-year-week-long-to-year-week-map []
    (reduce (fn [acc [abs-week year-week week year-week-long :as val]]
              (into acc
                    [[year-week-long [year-week week year-week-long abs-week]]]))
            {}
            (set (vals abs-week-map))))

#_(def year-week-long-to-year-week-map (init-year-week-long-to-year-week-map))

#_(defn week-year-from-year-week-long [year-week-long]
    (year-week-long-to-year-week-map year-week-long))

#_(tests

    ; TODO remove year, year-week and year-week-long
    ;      rename to abs-week-from-year-week-long
    (week-year-from-year-week-long 201001) := [2010 1 201001 1]
    (week-year-from-year-week-long 202322) := [2023 22 202322 700])


#_(defn get-abs-current-week
    "Returns current time as abs week: [abs-week year cal-week]"
    []
    (get-abs-week-old (t/date)))


#_(comment ; needed?
    (defprotocol ToLocal
      (as-local [this]))
    ;(as-date-time [this]))
    (extend-protocol ToLocal
      Instant
      (as-local [i] (bd/tick-date-to-native-date i))))

#_(comment
    (t/date (t/inst)))

#_(defprotocol YearWeekable
    (year-week [this])
    (year-of-year-week [this])
    (week-of-year-week [this]))

#_(extend-protocol YearWeekable
    LocalDateTime
    (year-week [dt] (get-abs-week (t/date dt)))
    (year-of-year-week [dt] (second (year-week dt)))
    (week-of-year-week [dt] (get (year-week dt) 2))
    LocalDate
    (year-week [d] (get-abs-week d))
    (year-of-year-week [d] (second (year-week d)))
    (week-of-year-week [d] (get (year-week d) 2))
    Instant
    (year-week [d] (get-abs-week (t/instant d)))
    (year-of-year-week [d] (second (year-week d)))
    (week-of-year-week [d] (get (year-week d) 2)))

#_(comment
    (type (bd/tick-date-to-native-date (t/today)))
    (t/date (t/instant "2023-03-03T00:00:00.000Z"))
    #_(get-abs-current-week)
    #_(get-abs-week (t/today))
    #_(year-week (t/date "2023-03-03"))
    #_(year-week (t/date-time "2023-03-03T14:30")))

#_(tests
    #_(year-of-year-week (t/date "2023-03-03")) := 2023
    #_(year-of-year-week (t/date-time "2023-03-03T14:30")) := 2023

    #_(week-of-year-week (t/date "2023-03-03")) := 9
    #_(week-of-year-week (t/date-time "2023-03-03T14:30")) := 9)

#_(defn weekify-element
    "An element is a map that contains key k, e.g. :k as LocalDate.
  Returns the map with an additional key:
  :k-cw will be there representing the week
  as abs-week as [abs-week year-of-week-year week]"
    ; TODO: change to get-abs-week NEW
    [element k]
    (assert (k element) (str "k needs to be a valid key: " k))
    (assert (or (t/date? (k element))
                (vector? (k element))
                (list? (k element)))
            (str "element needs to be a date or a vector or list of dates; element: " element))

    (let [week-k (keyword (clojure.string/join (into (vec (drop 1 (str k))) (vec "-cw"))))]
      (if (t/date? (k element))
        (assoc element
          week-k (get-abs-week-old (k element)))
        (assoc element
          week-k (map #(get-abs-week-old %) (k element))))))

#_(tests
    (weekify-element {:start (t/date "2023-01-01")} :start) := {:start (t/date "2023-01-01"), :start-cw [678 2022 52 202252]}
    (weekify-element
      {:start-end [(t/date "2023-01-01") (t/date "2024-01-01")]}
      :start-end) := {:start-end    [(t/date "2023-01-01") (t/date "2024-01-01")]
                      :start-end-cw [[678 2022 52 202252] [731 2024 1 202401]]})

#_(comment
    (keyword (clojure.string/join (into (vec (drop 1 (str :abc))) (vec "-cw")))))

#_(defn weekify
    "See weekify-element. Does it for a coll of elements."
    [coll]
    (map weekify-element coll))
#_(defn weeks-from-abs-weeks [start-week num-weeks]
    (vec (map (fn [e] (week-year-from-abs-week (+ start-week e 1)))
              (range num-weeks))))

#_(defn weeks-indicators [all-year-weeks]
    (map #(str (first %) "-" (second %)) all-year-weeks))

#_(tests
    (weeks-from-abs-weeks 0 3) := [[2010 1 201001] [2010 2 201002] [2010 3 201003]]
    (weeks-from-abs-weeks 701 1) := [[2023 24 202324]]
    (weeks-indicators (weeks-from-abs-weeks 0 3)) := '("2010-1" "2010-2" "2010-3")
    nil)



;;
;; SAME API, but uncached - in order to compare
;; all calls have -uc at the end.
;;
;; assumption: from epoch-day, beginning somewhere at 1.1.1970,
;; we have ALWAYS weeks of 7 days. That means, the absolute
;; calendar week abs-week can be calculated very simple.

#_(def dt t/date-time)

#_(comment

    (.toEpochDay (d "1970-01-01"))
    (.toEpochDay (d "1969-12-31")) ; works negative

    ;; lets try... ;; calender weeks start at, e.g.
    ;; 2010-01-04
    ;; 2010-01-11 +7
    (bc/pprint (bd/date-breakdown (d "1969-12-31")))
    (bc/pprint (bd/date-breakdown (d "2010-01-04")))

    ;; let's see, which date starts
    ;; abs-week 0...
    ;; Assuming, Monday 2010-01-04 starts week 1...
    ;; Its:
    ;; 2009-12-28
    (mapv bd/date-breakdown
          (list-of-all-days
            (dt "2009-12-28T00:00")
            8))

    nil)



#_(comment
    (bd/date-breakdown (d "1970-01-05")) ; => MONDAY
    (defn epoch-week-from-epoch-day [epoch-day]
      (let [epoch-day (+ epoch-day 2)
            epoch-day (if (neg? epoch-day)
                        (- epoch-day 6)
                        epoch-day)]
        (quot epoch-day 7)))

    (defn epoch-week [tick-date]
      (let [epoch-day  (.toEpochDay tick-date)
            epoch-week (epoch-week-from-epoch-day epoch-day)]
        epoch-week))

    (map (fn [val] {:epoch-day  val
                    :epoch-week (epoch-week-from-epoch-day val)
                    :date       (t/new-date val)})
         (range -20 15))

    :end)
;;                             30.12.1969              6.1.1970
;;                                      |  1.1.1970    |
;;                                      |    |         MONDAY
;;                                      |    |         |
;; EPOCH-DAY  -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9
;; EPOCH-WEEK  -2 -1 -1 -1 -1 -1 -1 -1  0  0 0 0 0 0 0 1 1 1

#_(def first-day-of-abs-week-0
    "the definition of the origin of abs-week,
  2009-12-28, put as epoch-day: 14606"
    (:epoch-day (bd/date-breakdown (d "2009-12-28"))))

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

#_(def get-abs-week-mem
    "abs-week memoized."
    (memoize get-abs-week))


;; first, lets test, if the calendar week is correct,
;; just dividing epoch-day by 7...
#_(defn cmp-abs-week [tick-date]
    (let [cw    (first (get-abs-week-old- tick-date))
          cw-uc (get-abs-week tick-date)]
      (assert (= cw cw-uc) (str "cw: " cw ", cw-uc: " cw-uc " should be equal for date: " tick-date)))
    :ok)

#_(tests

    ;; test one date
    (return-ex
      (cmp-abs-week (d "2011-01-04"))) := :ok

    ;; test maaaaany.
    (return-ex
      (do ; compare results of get-abs-week-old and get-abs-week
        (mapv cmp-abs-week
              (list-of-all-days (dt "2010-01-04T00:00")
                                10900))
        :ok)) := :ok

    :test-finished)


;;
;; PERFORMANCE
;;
;; simplest calc ist fastest in that access-profile
;; (randomly and wide distributed)
;;
;; A: "Elapsed time: 1224.800000 msecs"
;; B: "Elapsed time: 4623.200000 msecs"
;; C: "Elapsed time: 5023.100000 msecs"
#_(comment
    (let [days (-> (dt "2010-01-04T00:00")
                   (list-of-all-days 10900)
                   shuffle)]

      ; A: calc abs-week every time, based on .toEpochDays
      ; 271 msecs
      (println "A:")
      (time (dotimes [n 1e3]
              (mapv get-abs-week days)
              nil))

      ; B: memoize of A
      ; 906 msecs
      (println "\nB:")
      (time (dotimes [n 1e3]
              (mapv get-abs-week-mem days)
              nil))

      ; C: the pre-calculated map
      ; 906 msecs
      (println "\nC:")
      (time (dotimes [n 1e3]
              (mapv get-abs-week-old- days)
              nil)))

    :end)
