(ns belib.browser
  (:require [cljs.reader :as r]
            [clojure.pprint :refer [pprint]]
            [goog.functions]
            [goog.object :as gobj]))



(comment
  ;; see belib.date-time
  (defn now []
    (.getTime (js/Date.)))
  (now))

(defn dpr [sexpr]
  (pprint sexpr)
  sexpr)

(defn js-obj->clj-map
  "Uses the Google Closure object module to get the keys and values of any JavaScript Object
  and put them into a ClojureScript map"
  [obj]
  (zipmap (gobj/getKeys obj) (gobj/getValues obj)))

(comment
  (def json "{\"foo\": 1, \"bar\": 2, \"baz\": [1,2,3]}")
  (def a (.parse js/JSON json))
  (js->clj a)
  (js->clj a :keywordize-keys true)

  (def a2 (js-obj "foo" 1 "bar" 2))
  (js-obj->clj-map a2)
  (js->clj a2 :keywordize-keys true))

;;
;; debounce the simplest possible way in cljs
;; Trailing edge debouncing
;;
(defn debounced-later
  "Calls function only after interval of silence."
  [f interval]
  (goog.functions.debounce f interval))

(comment
  (def d (debounced-later #(println (.getTime (js/Date.))) 500))
  (d))

(defn fn-name [f]
  (let [s (str f)
        s (subs s 9 (clojure.string/index-of s "("))]
    (second (re-matches #"^.*\$([^\$]+)$" s))))

(defn debounced-now
  "Call f immediately and then at a highest frequency of interval milliseconds.
   Call the last call to f also with last recorded args at the end,
   of the intervall, even if it was early as second call in the beginning
   of the intervall."
  [f interval]
  ;(println "f:" f)
  ;(println "new debounce:" (fn-name f) ", time: " interval)
  (let [timer          (atom nil) ; if timer runs, block calls
        next-call-args (atom nil)]
    (letfn [(call-f []
              (reset! timer nil)
              ;(println "args:" @next-call-args)
              (when @next-call-args ; only, when there was a call after the first
                (let [tmp-args @next-call-args]
                  (reset! next-call-args nil) ; nil is signal for "called already"
                  (start-timer)
                  (apply f tmp-args))))
            (start-timer [] (reset! timer (js/setTimeout call-f interval)))] ; if timer finishes, call f with last args and start timer again
      (fn [& args]
        (reset! next-call-args (if args args '())) ; avoid nil if empty
        (when-not @timer
          (call-f))))))

(defn plus-print [n1 n2]
  (println n1 "+" n2 "=" (+ n1 n2) "  ( time =" (- (quot (.getTime (js/Date.)) 1000) 1686200000) ")")
  (+ n1 n2))


(comment
  (js/alert "abc")
  (plus-print 4 5)
  (def pr-plus-only-500-ms (debounced-now plus-print 3000))
  (do (pr-plus-only-500-ms 3 4)
      (pr-plus-only-500-ms 3 40)
      (pr-plus-only-500-ms 3 400))
  (do (pr-plus-only-500-ms 3 4000)
      (pr-plus-only-500-ms 3 40000)))


;;----------------------------------------------------------------------
;; local storage
;;----------------------------------------------------------------------

(defn prjs
  "Print javascript objects to consloe"
  [obj]
  (.log js/console obj))

(defn ls-set-item!
  "Set `key' in browser's localStorage to `val`."
  [key val]
  (.setItem (.-localStorage js/window) key val))

(defn ls-set-clj-item!
  "Set `key' in browser's localStorage to clj data `val`."
  [key val]
  (ls-set-item! key (pr-str val)))

(defn ls-get-item
  "Returns value of `key' from browser's localStorage."
  [key]
  (.getItem (.-localStorage js/window) key))

(defn ls-get-clj-item
  "Returns clj data value of `key' from browser's localStorage."
  [key]
  (r/read-string (ls-get-item key)))


(defn ls-remove-item!
  "Remove the browser's localStorage value for the given `key`"
  [key]
  (.removeItem (.-localStorage js/window) key))
