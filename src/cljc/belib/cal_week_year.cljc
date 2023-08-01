(ns belib.cal-week-year
  (:require [tick.core :as t]
            [hyperfiddle.rcf :refer [tests]]
            [belib.date-time :as bd]
            #?(:clj  [belib.test :as bt :refer [expect-ex]]
               :cljs [belib.test :as bt :refer-macros [expect-ex]])
            #?(:cljs [java.time :refer [LocalDateTime LocalDate Instant]])
            #?(:cljs [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break
                                                         clog_ clogn_ dbg_ dbgn_ break_]]
               :clj  [debux.cs.core :as d :refer [clog clogn dbg dbgn break
                                                  clog_ clogn_ dbg_ dbgn_ break_]]))
  #?(:clj (:import [java.time LocalDateTime LocalDate Instant])))

(hyperfiddle.rcf/enable! false)

(comment
  (dbgn (+ 3 (- 4 5))))



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

(defn date-infos-as-long-raw [tick-date]
  (assert (bd/date? tick-date))
  (let [[week-based-year week-of-week-based-year] (bd/iso-week-year tick-date)]
    (-> week-based-year
        (* 100)
        (+ week-of-week-based-year))))

(def date-infos-as-long (memoize date-infos-as-long-raw))

(tests
  (date-infos-as-long (t/date "2014-12-31")) := 201501)

(comment
  (map t/date (let [start (t/date-time "2010-01-04T00:00")]
                (t/range
                  start
                  (t/>> start (t/new-duration (+ (* 365 30) 4) :days))
                  (t/new-period 1 :days)))))

(defn- calc-abs-weeks
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
               (conj result [c-date [c-abs-week c-year c-week-of-year]])))
      result)))

(defn list-of-all-days []
  (map t/date (let [start (t/date-time "2010-01-04T00:00")]
                (t/range
                  start
                  (t/>> start (t/new-duration (+ (* 365 30) 4) :days))
                  (t/new-period 1 :days)))))


(tests
  (take 2 (list-of-all-days)) :=
  [(t/date "2010-01-04") (t/date "2010-01-05")]

  (take-last 2 (list-of-all-days)) :=
  [(t/date "2039-12-30") (t/date "2039-12-31")])

(defn- init-absolut-weeks
  "Creates map with
  key = 'local date' and
  value = [absolute-week year week-of-year]"
  []
  (let [all-days-with-week (map (fn [d] (concat [d] (bd/iso-week-year d)))
                                (list-of-all-days))]
    (into
      (sorted-map)
      (calc-abs-weeks all-days-with-week))))

;; lookup-table
(defonce abs-week-map (init-absolut-weeks))

(tests
  (count abs-week-map) := 10954) ; about 11.000 days

(defn init-abs-week-to-year-week-map []
  (reduce (fn [acc  val]
            (into acc
                  [[(first val) [(second val) (last val)]]]))
          {}
          (set (vals abs-week-map))))

(def abs-week-to-year-week-map (init-abs-week-to-year-week-map))

(defn week-year-from-abs-week [abs-week]
  (abs-week-to-year-week-map abs-week))

(tests
  (week-year-from-abs-week 1) := [2010 1])



(def first-date (first (first abs-week-map)))
(def last-date (first (last abs-week-map)))

(defn get-abs-week
  "Delivers an absolute week number that is linear over all years.
  Returns [absolute-week-since-2010-01-04 year week-of-year]
  Starting with 2010-01-04, which is the start of week 1 in that year.
  Ending with 2039-12-31. Before and after that dates, nil results
  will be returned."
  [tick-date]
  (assert (bd/date? tick-date))
  (assert (t/>= tick-date first-date) "date has to be after 2010-01-04")
  (assert (t/<= tick-date last-date) "date has to be before 2039-12-31")
  (abs-week-map tick-date))

(tests
  (get-abs-week (t/date "2030-09-27")) := [1082 2030 39]
  (get-abs-week (t/date "2010-01-04")) := [1 2010 1]
  (get-abs-week (t/date "2039-12-31")) := [1565 2039 52]
  (expect-ex (get-abs-week (t/date "2040-01-01"))) := #?(:clj AssertionError :cljs js/Error)
  (expect-ex (get-abs-week (t/date "2010-01-03"))) := #?(:clj AssertionError :cljs js/Error)
  nil)


(defn get-abs-current-week
  "Returns current time as abs week: [abs-week year cal-week]"
  []
  (get-abs-week (t/date)))


(comment ; needed?
  (defprotocol ToLocal
    (as-local [this]))
  ;(as-date-time [this]))
  (extend-protocol ToLocal
    Instant
    (as-local [i] (bd/tick-date-to-native-date i))))

(comment
  (t/date (t/inst)))

(defprotocol YearWeekable
  (year-week [this])
  (year-of-year-week [this])
  (week-of-year-week [this]))

(extend-protocol YearWeekable
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

(comment
  (type (bd/tick-date-to-native-date (t/today)))
  (t/date (t/instant "2023-03-03T00:00:00.000Z"))
  (get-abs-current-week)
  (get-abs-week (t/today))
  (year-week (t/date "2023-03-03"))
  (year-week (t/date-time "2023-03-03T14:30")))

(tests
  (year-of-year-week (t/date "2023-03-03")) := 2023
  (year-of-year-week (t/date-time "2023-03-03T14:30")) := 2023

  (week-of-year-week (t/date "2023-03-03")) := 9
  (week-of-year-week (t/date-time "2023-03-03T14:30")) := 9)

(defn weekify-element
  "An element is a map that contains a :start and :end as LocalDate.
  Returns the map with two additional keys:
  :start-week and :end-week will be there representing the start and end week
  as abs-week as integer."
  [element k]
  (assert (k element) (str "k needs to be a valid key: " k))
  (assert (or (bd/date? (k element))
              (vector? (k element))
              (list? (k element)))
          (str "element needs to be a date or a vector or list of dates; element: " element))

  (let [week-k (keyword (clojure.string/join (into (vec (drop 1 (str k))) (vec "-cw"))))]
    (if (bd/date? (k element))
      (assoc element
        week-k (get-abs-week (k element)))
      (assoc element
        week-k (map #(get-abs-week %) (k element))))))

(tests
  (weekify-element {:start (t/date "2023-01-01")} :start) := {:start (t/date "2023-01-01"), :start-cw [678 2022 52]}
  (weekify-element
    {:start-end [(t/date "2023-01-01") (t/date "2024-01-01")]}
    :start-end) := {:start-end    [(t/date "2023-01-01") (t/date "2024-01-01")]
                    :start-end-cw [[678 2022 52] [731 2024 1]]})

(comment
  (keyword (clojure.string/join (into (vec (drop 1 (str :abc))) (vec "-cw")))))

#_(defn weekify
    "See weekify-element. Does it for a coll of elements."
    [coll]
    (map weekify-element coll))
