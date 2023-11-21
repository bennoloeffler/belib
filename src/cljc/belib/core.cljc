(ns belib.core
  (:require [tick.core :as t]
            [hyperfiddle.rcf :refer [tests]]
            #?(:clj  [belib.test :as bt :refer [expect-ex return-ex return:error-if-ex]]
               :cljs [belib.test :as bt :refer-macros [expect-ex return-ex return:error-if-ex]])
            #?(:clj  [snitch.core :refer [defn* defmethod* *fn *let]]
               :cljs [snitch.core :refer-macros [defn* defmethod* *fn *let]])
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [time-literals.read-write]))
;[belib.time+ :as ti]))

;;
;; MORE TOOLS
;; see: https://github.com/weavejester/medley
;;

#?(:cljs (time-literals.read-write/print-time-literals-cljs!)
   :clj  (time-literals.read-write/print-time-literals-clj!))

(hyperfiddle.rcf/enable! false)

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

; TODO adapt signature
; isn't there a more
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

(tests
  (deep-merge-with + {:a {:b 1} :c 1} {:a {:b 2} :d 2})
  := {:a {:b 3}, :c 1, :d 2}

  "use first value only"
  (deep-merge-with (fn [v1 _] v1) {:a {:b 1} :c 1} {:a {:b 2} :d 2})
  := {:a {:b 1}, :c 1, :d 2}

  "use second value only"
  (deep-merge-with (fn [_ v2] v2) {:a {:b 1} :c 1} {:a {:b 2} :d 2})
  := {:a {:b 2}, :c 1, :d 2}

  (deep-merge-with (fn [v1 v2] (+ 10 (+ v1 v2))) {:a {:b 1} :c 1} {:a {:b 2} :d 2})
  := {:a {:b 13}, :c 1, :d 2}

  :end-test)

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
  (*let [id1 (next-local-id)
         id2 (next-local-id)]
        id1 :<> id2))
(comment
  id1 ; just use them as defs...
  id2)


(def p #?(:cljs cljs.pprint/pprint
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
  are not yet available, when adding.
  So adding elements will be done based
  on the key value of the new element ONLY."
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

;; this is for pow - especially in js
(defn bigint?
  "Returns true if n is a BigInt"
  [n]
  #?(:clj  (instance? clojure.lang.BigInt n)
     :cljs (= js/BigInt (type n))))

(defn pow
  "(pow 2 3) = (* 2 2 2),
  even with very big numbers.
  Works with ints.
  ATTENTION: delivers js/BigInt in js."
  [n x]
  (assert (or (bigint? n) (int? n)))
  (assert (or (bigint? x) (int? x)))
  #?(:cljs (let [n (js/BigInt n)
                 x (js/BigInt x)]
             (loop [result (js/BigInt 1) counter (js/BigInt 0)]
               ;(println n x nn xx)
               (if (= x counter)
                 result
                 (recur (* result n)
                        (+ counter (js/BigInt 1))))))
     :clj  (loop [result  1N
                  counter 0N]
             ;(println n x nn xx)
             (if (= x counter)
               result
               (recur (* result n)
                      (+ counter 1))))))

;(pow (js/BigInt 4) (js/BigInt 4))

(comment
  (bigint? 15)
  (bigint? 12N)
  (def bi (js/BigInt 10))
  (* bi bi)
  (= js/BigInt (type (js/BigInt 12)))
  (bigint? (js/BigInt 12))
  ; error in js:
  (+ 4 (pow 4 4)) ; cant get BigInt back to Number
  ;(+ 4 (.-asIntN (pow 4 4)))
  (+ 4 (js/Number (pow 4 4)))
  (pow (js/BigInt 10) (js/BigInt 1000))
  ;(+ 9007199254740991 2)
  ;(+ 9007199254740991N 2N)
  ;(instance? js/BigInt 234234234234234243243N)
  ;(type 234234232345324523452345324523452345234523454234234243234213434534534534534543N)
  ;(bigint? 23N)

  :end)



#?(:clj
   (tests

     "just the normal test"
     (pow 8 0) := 1
     (pow 8 1) := 8
     (pow 8 2) := 64

     "convert back"
     (+ 6 (pow 8 2)) := 70

     "big numbers"
     (pow 9223453453423423423423423423423698N 1)
     := 9223453453423423423423423423423698N

     :end-test)

   :cljs
   (tests

     "just the normal test"
     (pow 8 0) := (js/BigInt 1)
     (pow 8 1) := (js/BigInt 8)
     (pow 8 2) := (js/BigInt 64)

     "convert back"
     (+ 6 (js/Number (pow 8 2))) := 70


     (pow (js/BigInt 9223453453423423423423423423423698) 1)
     := (js/BigInt 9223453453423423423423423423423698)

     ; DOES NOT WORK???
     ;(pow (js/BigInt 9223453453423423423423423423423698) 2)
     ;:= (js/BigInt 85072093607468470411023697411718203977321564214944340144834650046464)

     :end-test))

(comment
  (parse-long "10")
  (long (bigdec "100.0")))

(defn parse-long [x]
  #?(:cljs (js/parseInt x 10) :clj (Long/valueOf x)))

#?(:clj
   (tests
     (def longest (str Long/MAX_VALUE))
     (parse-long longest) := Long/MAX_VALUE ;9223372036854775807
     :end-test))

#?(:cljs
   (tests
     (def longest (str (.-MAX_SAFE_INTEGER js/Number)))
     (parse-long longest) := (.-MAX_SAFE_INTEGER js/Number) ; 9007199254740991
     :end-test))

(defn long-str [& strings] (str/join "\n" strings))
(defn long-str-one-line [& strings] (str/join "" strings))

(defn str-re
  "Convert regex to string that may be appended to other regex str.
   This is needed for cljs."
  [re]
  #?(:cljs (subs (str re) 1 (dec (count (str re))))
     :clj  (str re)))

(defn long-re
  "Append different regexes to one:
  (long-re #\"\\d|\"
           #\"\\d\")"
  [& regexes] (re-pattern (str/join "" (map str-re regexes))))


(tests
  "compare patterns only by string. They are not compared by value, but by identity."
  (str-re (re-pattern (str-re #"\d"))) := (str-re #"\d")

  "append regexes"
  (str-re (long-re #"\d|"
                   #"\d")) := (str-re #"\d|\d")

  "append strings"
  (long-str "one long"
            "two long") := "one long\ntwo long"

  (long-str-one-line "one-long, "
                     "two-long") := "one-long, two-long"

  :end-tests)


(defn swap
  "Swaps values of idx1 and idx2 in vector.
   (swap [0 1 2 3] 1 2) ;=> [0 2 1 3]"
  [v idx1 idx2]
  (if v
    (if (= idx1 idx2)
      v
      (let [val1 (get v idx1)
            val2 (get v idx2)]
        (-> v
            (assoc idx1 val2)
            (assoc idx2 val1))))
    nil))

(tests
  (swap [0 1 2 3] 1 2) := [0 2 1 3]
  (swap [0 1 2 3] 1 1) := [0 1 2 3]
  (swap [0 1 2 3] 0 3) := [3 1 2 0]
  (swap [0 1 2 3] 3 0) := [3 1 2 0]
  (swap [0 1 2 3] 0 4) := [nil 1 2 3 0]
  (swap [0 1 2 3] 4 0) := [nil 1 2 3 0]

  (swap nil -1 2) := nil
  :end-tests)

(defn balanced? [s]
  (->> s
       ;; remove non-bracket characters
       (filter #{\[ \] \( \) \{ \}})

       ;; reduce down to an empty or non-empty vector
       (reduce
         (fn [stack item]
           (cond
             (#{\( \{ \[} item)
             (conj stack item)

             (and (#{\( \{ \[} (last stack))
                  (= ({\) \(, \} \{, \] \[} item) (last stack)))
             (pop stack)

             :else (conj stack item)))
         [])
       ;; return whether we have any unbalanced brackets
       empty?))

(tests
  "balanced"
  (balanced? "(s(b [ asdfsaf ] {} ))") := true
  (balanced? "") := true
  (balanced? "()(") := false
  (balanced? "(})") := false)




(defn idx-map
  "Returns a map of elements of the vector vec with the idx as key."
  [vec]
  (assert (vector? vec))
  (into {} (map-indexed (fn [idx entity] [idx entity]) vec)))

(tests
  (idx-map [{:a :b} {:c :d}])
  := {0 {:a :b}, 1 {:c :d}}

  :end-tests)

(defn id-map
  "Returns a map with the entity-ids as keys, like, e.g.:
  {2 {:eid 2
      :age 16
      :male false}
   3 {:eid 3
      :age 16
      :male true}}

  Even if the map has different keys:
  {2222 {:eid 2 :age 16 :male false} 3333 {:eid 3 :age 16 :male true}}
  Will result in:
  {2 {:eid 2 :age 16 :male false} 3 {:eid 3 :age 16 :male true}}

   you may also need such a lookup-table from a vector of entities:
   [{:eid 2
     :age 16
     :male false}
    {:eid 3
     :age 16
     :male true}]
   "
  [coll entity-id-key]
  (let [data (if (map? coll)
               (vals coll)
               coll)]
    (into {} (mapv (fn [{entity-id entity-id-key :as entity}]
                     (assert (some? entity-id) (str "every entity needs to be a map with entity-id called: " entity-id-key))
                     [entity-id entity])
                   data))))

(tests

  (id-map {22222 {:eid  2
                  :age  16
                  :male false}
           33333 {:eid  3
                  :age  16
                  :male true}} :eid)
  := {2 {:eid 2, :age 16, :male false}, 3 {:eid 3, :age 16, :male true}}

  (id-map [{:eid  2
            :age  16
            :male false}
           {:eid  3
            :age  16
            :male true}] :eid)
  := {2 {:eid 2, :age 16, :male false}, 3 {:eid 3, :age 16, :male true}}

  (-> [{;:eid  2 ; MISSING!
        :age  16
        :male false}
       {:eid  3
        :age  16
        :male true}]
      (id-map :eid)
      return-ex
      ex-message)
  := "Assert failed: every entity needs to be a map with entity-id called: :eid\n(some? entity-id)"

  :end-tests)

(defn vec-remove-idx
  "remove elem at pos in coll"
  [pos coll]
  ;(assert (vector? coll))
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn vec-remove-elem
  "Remove FIRST element that equals elem from vector."
  [elem coll]
  ;(assert (vector? coll))
  (let [pos (.indexOf coll elem)]
    (if (>= pos 0)
      (vec-remove-idx pos coll)
      coll)))

(tests
  (vec-remove-idx 2 ["1" "2" "3" "4" "5"]) := ["1" "2" "4" "5"]
  (vec-remove-elem "2" ["1" "2" "3" "4" "5"]) := ["1" "3" "4" "5"]
  (vec-remove-elem "X" ["1" "2" "3" "4" "5"]) := ["1" "2" "3" "4" "5"]

  :end-tests)

(defn remove-elem
  "Remove ALL elements that equals elem from vector.
  coll may be list, set or vector.
  Returns a collection of type coll."
  [elem coll]
  (let [removed (remove #(= elem %) coll)]
    (if (list? coll)
      removed
      (into (empty coll) removed))))

(tests
  (remove-elem "2" ["1" "2" "3" "4" "5"]) := ["1" "3" "4" "5"]
  (remove-elem "2" '("1" "2" "3" "4" "5")) := '("1" "3" "4" "5")
  (remove-elem "2" #{"1" "2" "3" "4" "5"}) := #{"1" "3" "4" "5"}
  :end-tests)

#_(tests
    "remove-elem is more than 3x faster than vec-remove-element"
    (let [fast   (->> (remove-elem "2" ["1" "2" "3"])
                      (ti/time-data 100)
                      :time-per-call-ns)
          slow   (->> (vec-remove-elem "2" ["1" "2" "3"])
                      (ti/time-data 100)
                      :time-per-call-ns)
          faster (< (* 3 fast) slow)]
      faster) := true

    :end-tests)


