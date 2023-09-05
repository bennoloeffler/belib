(ns belib.browser
  (:require [cljs.reader :as r]
            [clojure.pprint :refer [pprint]]
    ;https://funcool.github.io/cuerdas/latest/user-guide.html
            [cuerdas.core :as str]
            [goog.functions]
            [goog.object :as gobj]
            [hyperfiddle.rcf :refer [tests]]))


(hyperfiddle.rcf/enable! false)

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
  (cljs.core/js->clj a)
  (cljs.core/js->clj a :keywordize-keys true)

  (def a2 (js-obj "foo" 1 "bar" 2))
  (js-obj->clj-map a2)
  (cljs.core/js->clj a2 :keywordize-keys true)

  nil)

(defn fn-name
  "Return the name of the function fn."
  [fn]
  (let [s (str fn)
        s (subs s 9 (clojure.string/index-of s "("))]
    (second (re-matches #"^.*\$([^\$]+)$" s))))

;;---------------------------------------------------------------
;; debouncing
;;---------------------------------------------------------------

(defn debounced-later
  "Calls function only after interval of silence.
  Debounce the simplest possible way in cljs.
  Trailing edge debouncing."
  [f interval]
  (goog.functions.debounce f interval))

(comment
  (def d (debounced-later #(println (.getTime (js/Date.))) 500))
  (d))

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

;;------------------------------------------------------------
;; react events
;;------------------------------------------------------------

(def all-events-raw
  "Lists all react events in camelCase.
  See: https://legacy.reactjs.org/docs/events.html
  Use (.stopPropagation e) or (.preventDefault e)"

  "onCopy onCut onPaste

  onKeyDown onKeyPress onKeyUp

  onFocus onBlur

  onChange onInput onInvalid onReset onSubmit

  onError onLoad

  onClick onContextMenu onDoubleClick

  onDrag onDragEnd onDragEnter
  onDragExit onDragLeave onDragOver onDragStart onDrop

  onMouseDown onMouseEnter onMouseLeave
  onMouseMove onMouseOut onMouseOver onMouseUp

  onPointerDown onPointerMove onPointerUp onPointerCancel
  onGotPointerCapture onLostPointerCapture onPointerEnter
  onPointerLeave onPointerOver onPointerOut

  onSelect

  onTouchCancel onTouchEnd onTouchMove onTouchStart

  onScroll

  onWheel

  onAbort onCanPlay onCanPlayThrough onDurationChange onEmptied onEncrypted
  onEnded onError onLoadedData onLoadedMetadata onLoadStart onPause onPlay
  onPlaying onProgress onRateChange onSeeked onSeeking onStalled onSuspend
  onTimeUpdate onVolumeChange onWaiting

  onLoad onError

  onAnimationStart onAnimationEnd onAnimationIteration

  onToggle")

(def all-events
  "All react events as kebab keywords."
  (as-> all-events-raw $
        (clojure.string/split $ #"[ \n]")
        (filter #(not= "" %) $)
        #_(map csk/->kebab-case-keyword $)
        (map #(keyword (str/kebab %)) $)))

(comment
  (keyword (str/kebab "abCdeFgh")))


(defn merge-event [properties-map as-keyword]
  (let [evt-handler (fn [event] (println (str as-keyword)))]
    (merge properties-map
           {as-keyword evt-handler})))

(def all-events-map
  "All react events as default event listeners:
  {:on-drag-exit (fn [event] (println \":on-drag-exit\"))
   ...}
   Used with merge in hiccup properties:
   [:div (merge {:some-prop :some-val}
                all-events-map)
          \"some-text\""
  (reduce merge-event
          {}
          all-events))

(defn filter-events-map
  "Add react events with a regex.
  Example
  [:div (merge {:some-prop :some-val}
               (filter-events-map #\"mo|dr\")
        \"some-text\""
  [re]
  (let [filtered-events (filter
                          #(re-find re (str %))
                          all-events)]
    (println "ADDING react events to test them interactively:")
    (println filtered-events)
    (reduce merge-event
            {}
            filtered-events)))

(tests
  (keys (filter-events-map #"copy|suspend")) := [:on-copy :on-suspend])

