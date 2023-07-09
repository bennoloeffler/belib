(ns belib.core
  (:require [tick.core :as t]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.spec.alpha :as s]))

(hyperfiddle.rcf/enable! true)


#_(defn debug-tools
    "get all the needed tools for debugging
  required."
    []
    (println "put that in your projects.clj...")
    (println "[io.github.erdos/erdos.assert \"0.2.3\"]")
    (println "[philoskim/debux \"0.8.3\"]\n")
    (require '[debux.core :refer :all]
             '[erdos.assert :as ea]))

#_(defmacro compile-time
    "returns the date and time of the
  last compilation of the function,
  where this macro is called like:
  (compile-time)"
    []
    (str (t/date-time)))

(defn test-belib
  "get a string to see that the lib works
  and when it was compiled.
  Especially useful, when used
  with checkouts feature from lein."
  []
  (str "BELs lib seems to work.\n"))
       ;"Compiled: " (compile-time)))

(comment
  (println (test-belib)))

(defn map-kv
  "map the values of a map:
  (map-kv {:a 1 :b 2} inc))
  => {:a 2, :b 3}"
  [m f]
  (reduce-kv #(assoc %1 %2 (f %3)) {} m))

(comment
  (map-kv {:a 1 :b 2} inc))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.
  (deep-merge-with + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
                     {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))

(defn dissoc-in [m ks k]
  (update-in m ks dissoc k))

#?(:clj
   (defonce local-id (atom (System/currentTimeMillis)))
   :cljs
   (defonce local-id (atom (.now js/Date.))))


(defn next-local-id
  "WARNING: just a sequence of
  longs, starting with current
  timestamp in milliseconds.
  So, unique on ONE LOCAL machine!"
  []
  (swap! local-id inc))

(tests
  (let [id1 (next-local-id)
        id2 (next-local-id)]
    id1 :<> id2))

(def pprint #?(:cljs cljs.pprint/pprint
               :clj  clojure.pprint/pprint))


#_(tests
    (expect-ex (assert false)) := AssertionError
    (expect-ex (/ 1 0)) := ArithmeticException
    (expect-ex ArithmeticException (/ 1 0)) := true)



