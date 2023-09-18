(ns belib.core
  (:require [tick.core :as t]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.spec.alpha :as s]
            [time-literals.read-write]))


#?(:cljs (time-literals.read-write/print-time-literals-cljs!)
   :clj  (time-literals.read-write/print-time-literals-clj!))

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


;;-------------------------------------------
;; SORTING nested maps by keys in nested maps
;;-------------------------------------------

(defn sorted-map-by-keys
  "Sort map by up to 3 nested keys:
  sort-by-key-1 then-by-key-2 then-by-key-3
  Finally by map key, if sort keys deliver equality.
  ATTENTION: THIS WONT WORK FOR assoc, assoc-in or update,
  because the values of the keys to make the comparison with
  are not yet available. So adding elements will be done based
  on the key value ONLY."
  ([m sort-by-key-1 then-by-key-2 then-by-key-3]
   (into (sorted-map-by (fn [key-a key-b]
                          (- (compare [(get-in m [key-b sort-by-key-1])
                                       (get-in m [key-b then-by-key-2])
                                       (get-in m [key-b then-by-key-3])
                                       key-b]
                                      [(get-in m [key-a sort-by-key-1])
                                       (get-in m [key-a then-by-key-2])
                                       (get-in m [key-a then-by-key-3])
                                       key-a]))))
         m))
  ([m sort-by-key then-by-key-2]
   (sorted-map-by-keys m sort-by-key then-by-key-2 nil))
  ([m sort-by-key]
   (sorted-map-by-keys m sort-by-key nil nil))
  ([m]
   (sorted-map-by-keys m nil nil nil)))

(comment
  (into (sorted-map-by >) {:b "c" :a "x" :c "b"})
  (type (sorted-map-by-keys {} :nix))
  (sorted-map-by-keys {:E {:sequence-num 1 :val 23}
                       :B {:sequence-num 2 :val 22}
                       :C {:sequence-num 2 :val 53}
                       :D {:sequence-num 5 :val 73}
                       :F {:sequence-num 1 :val 10}
                       :A {:sequence-num 1 :val 11}}
                      :sequence-num :val))

(tests
  (let [m {:E {:sequence-num 1 :val 23 :part 1}
           :B {:sequence-num 2 :val 22 :part 1}
           :C {:sequence-num 2 :val 53 :part 2}
           :D {:sequence-num 5 :val 73 :part 3}
           :F {:sequence-num 1 :val 10}
           :A {:sequence-num 1 :val 11}}]
    ; sort just by key of map, when no other key is spefied
    (keys (sorted-map-by-keys m)) := '(:A :B :C :D :E :F)
    ; sort just by key of map, if specified key is not available
    (keys (sorted-map-by-keys m :nothing)) := '(:A :B :C :D :E :F)
    ; sort by the key given
    (keys (sorted-map-by-keys m :val)) := '(:F :A :B :E :C :D)
    ; if key is not ordered, use key of map: :A :E :F
    (keys (sorted-map-by-keys m :sequence-num)) := '(:A :E :F :B :C :D)
    ; sort first by first key, second by second
    (keys (sorted-map-by-keys m :sequence-num :val)) := '(:F :A :E :B :C :D)
    ; sorting by a partial key: nil is smaller than 3 (compare nil 3) := -1
    ; so the ones without key come first and are sorted by map key
    (keys (sorted-map-by-keys m :part)) := '(:A :F :B :E :C :D)
    ; sorting partial key, sorting may fall back to map key (:A :F)
    (keys (sorted-map-by-keys m :part :sequence-num)) := '(:A :F :E :B :C :D)

    ; ATTENTION: won't work. :val=12 should be between :A and :B
    ; like '(:F :A :X :B :E :C :D) but isn't: '(:X :F :A :B :E :C :D)
    (keys (assoc (sorted-map-by-keys m :val) :X {:val 12})) := '(:X :F :A :B :E :C :D)))

(defn bigint?
  "Returns true if n is a BigInt"
  [n] (instance? clojure.lang.BigInt n))

(defn pow
  "(pow 2 3) = (* 2 2 2),
  even with very big numbers.
  Works with ints."
  [n x]
  (assert (or (int? n) (bigint? n)))
  (assert (or (int? x) (bigint? x)))
  (loop [nn 1N xx 0N]
    ;(println n x nn xx)
    (if (= x xx)
      nn
      (recur (* nn n) (inc xx)))))

(tests
  (pow 8 0) := 1
  (pow 8 1) := 8
  (pow 8 2) := 64
  (pow 9223453453423423423423423423423698N 1) := 9223453453423423423423423423423698N

  :end-test)





