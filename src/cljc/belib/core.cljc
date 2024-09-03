(ns belib.core
  (:require [tick.core :as t]
            [hyperfiddle.rcf :refer [tests]]
            #?(:clj  [belib.test :as bt :refer [expect-ex return-ex return:error-if-ex]]
               :cljs [belib.test :as bt :refer-macros [expect-ex return-ex return:error-if-ex]])
            #?(:clj  [snitch.core :refer [defn* defmethod* *fn *let]]
               :cljs [snitch.core :refer-macros [defn* defmethod* *fn *let]])
            [clojure.spec.alpha :as s]
            [borkdude.deflet :refer [deflet]]
            [clojure.string :as str]
            [time-literals.read-write]
            [dom-top.core :as dt]
            [swiss.arrows :as sa])) ;:refer [-<> -<>>]]))


;[belib.time+ :as ti]))

;;
;; MORE TOOLS
;; see: https://github.com/weavejester/medley
;;

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

(defn filter-kv [pred map]
  (reduce-kv (fn [accumulator key value]
               (if (pred key value)
                 (assoc accumulator key value)
                 accumulator)) {} map))

(tests
  (filter-kv (fn [key _]
               (not (= key "a"))) {"a" {:some "a"}
                                   "b" {:some "b"}
                                   "c" {:some "c"}})

  := {"b" {:some "b"}
      "c" {:some "c"}})

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

(defn long-str
  "Join strings with newline.
  (long-str \"one long\"
            \"two long\")"
  [& strings] (str/join "\n" strings))

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


(defmacro loopr
  "SEE: https://aphyr.com/posts/360-loopr-a-loop-reduction-macro-for-clojure
  SEE: https://github.com/aphyr/dom-top

  Like `loop`, but for reducing over (possibly nested) collections. Compared to
  `loop`, makes iteration implicit. Compared to reduce, eliminates the need for
  nested reductions, fn wrappers, and destructuring multiple accumulators.
  Compared to `for`, loopr is eager, and lets you carry accumulators.

  Takes an initial binding vector for accumulator variables, (like `loop`);
  then a binding vector of loop variables to collections (like `for`); then a
  body form, then an optional final form. Iterates over each element of the
  collections, like `for` would, and evaluates body with that combination of
  elements bound.

  Like `loop`, the body should generally contain one or more (recur ...) forms
  with new values for each accumulator. Any non-recur form in tail position
  causes loopr to return that value immediately.

  When the loop completes normally, loopr returns:

  - The value of the final expression, which has access to the accumulators, or
  - If no `final` is given...
    - With zero accumulators, returns `nil`
    - With one accumulator, returns that accumulator
    - With multiple accumulators, returns a vector of each.

  For example,

    (loopr [sum 0]
           [x [1 2 3]]
      (recur (+ sum x)))

  returns 6: the sum of 1, 2 and 3.

  This would typically be written as `(reduce + [1 2 3])`, and for single
  accumulators or single loops using `reduce` or `loop` is often more concise.
  Loopred's power comes from its ability to carry multiple accumulators and to
  traverse multiple dimensions. For instance, to get the mean of all elements
  in a matrix:

    (loopr [count 0
            sum   0]
           [row [[1 2 3] [4 5 6] [7 8 9]]
            x   row]
      (recur (inc count) (+ sum x))
      (/ sum count))
    ; returns 45/9 = 5

  Here, we have a body which recurs, and a final expression `(/ sum count)`,
  which is evaluated with the final value of the accumulators. Compare this to
  the equivalent nested reduce:

    (let [[sum count] (reduce (fn [[count sum] row]
                                (reduce (fn [[count sum] x]
                                          [(inc count) (+ sum x)])
                                        [count sum]
                                        row))
                              [0 0]
                              [[1 2 3] [4 5 6] [7 8 9]])]
      (/ sum count))

  This requires an enclosing `let` binding to transform the loop results, two
  calls to reduce, each with their own function, creating and destructuring
  vectors at each level, and keeping track of accumulator initial values far
  from their point of use. The structure of accumulators is encoded in five
  places instead of two, which makes it harder to change accumulators later.
  It also requires deeper indentation. Here's the same loop expressed as a
  flat `loop` over seqs:

    (loop [count 0
           sum   0
           rows  [[1 2 3] [4 5 6] [7 8 9]]
           row   (first rows)]
      (if-not (seq rows)
        (/ sum count)       ; Done with iteration
        (if-not (seq row)   ; Done with row; move on to next row
          (recur count sum (next rows) (first (next rows)))
          (let [[x & row'] row]
            (recur (inc count) (+ sum x) rows row')))))

  This version is less indented but also considerably longer, and the
  interweaving of traversal machinery and accumulation logic makes it
  difficult to understand. It is also significantly slower than the nested
  `reduce`, on account of seq allocation--vectors can more efficiently reduce
  over their internal structure.

  Depending on how many accumulators are at play, and which data structures are
  being traversed, it may be faster to use `loop` with an iterator, `loop` with
  `aget`, or `reduce` with a function. loopr compiles to (possibly nested)
  `reduce` when given a single accumulator, and to (possibly nested) `loop`
  with mutable iterators when given multiple accumulators. You can also control
  the iteration tactic for each collection explicitly:

    (loopr [count 0
            sum   0]
           [row [[1 2 3] [4 5 6] [7 8 9]] :via :reduce
            x   row                       :via :iterator]
      (recur (inc count) (+ sum x))
      (/ sum count))

  This compiles into a `reduce` over rows, and a `loop` over each row using an
  iterators. For array iteration, use `:via :array`:

    (loopr [sum 0]
           [x (long-array (range 10000)) :via :array]
           (recur (+ sum x)))
    ; => 49995000

  Note that alength/aget are *very* sensitive to type hints; use `lein check`
  to ensure that you're not using reflection, and add type hints as necessary.
  On my older xeon, this is roughly an order of magnitude faster than (reduce +
  longs). For nested array reduction, make sure to hint inner collections, like
  so:

    (loopr [sum 0]
           [row                        matrix :via :array
            x   ^\"[Ljava.lang.Long;\" row    :via :array]
           (recur (+ sum x)))))

  Like `loop`, `loopr` supports early return. Any non `(recur ...)` form in
  tail position in the body is returned immediately, without visiting any other
  elements in the collection(s). To search for the first odd number in
  collection, returning that number and its index:

    (loopr [i 0]
           [x [0 3 4 5]]
           (if (odd? x)
             {:i i, :x x}
             (recur (inc i))))
    ; => {:i 1, :x 3}

  When no accumulators are provided, loopr still iterates, returning any
  early-returned value, or the final expression when iteration completes, or
  `nil` otherwise. Here we find an key in a map by value. Note that we can also
  destructure in iterator bindings.

    (loopr []
           [[k v] {:x 1, :y 2}]
           (if (= v 2)
             k
             (recur))
           :not-found)
    ; => :y"
  [& forms]
  `(dt/loopr ~@forms))


(tests
  (loopr [add 0
          mult 1] ; first vector of bindings is the accumulator(s)
         [a (range 1 4)
          b (range 1 4)] ; second binding is the sequence(es) to iterate over, like for
         (do
           ;(println a b)
           (recur (+ add a b)
                  (* mult a b))))
  := [36 46656] ; the both final accumulators
  :end-tests)


(defmacro assert+
  "SEE: https://github.com/aphyr/dom-top

   Like Clojure assert, but throws customizable exceptions (by default,
   IllegalArgumentException), and returns the value it checks, instead of nil.

   Clojure assertions are a little weird. Syntactically, they're a great
   candidate for runtime validation of state--making sure you got an int instead
   of a map, or that an object you looked up was present. However, they don't
   *return* the thing you pass them, which makes it a bit akward to use them in
   nested expressions. You typically have to do a let binding and then assert.
   So... let's return truthy values! Now you can

       (assert+ (fetch-person-from-db :liu)
                \"Couldn't fetch Liu!\")

   Moreover, Clojure assertions sensibly throw AssertionError. However,
   AssertionError is an error that \"should never occur\" and \"a reasonable
   application should not try to catch.\" There are LOTS of cases where you DO
   expect assertions to fail sometimes and intend to catch them: for instance,
   validating user input, or bounds checks. So we're going to throw
   customizable exceptions.

   Oh, and you can throw maps too. Those become ex-infos.

   (assert+ (thing? that)
            {:type   :wasn't-a-thing
             :I'm    [:so :sorry]})"
  [& forms]
  `(dt/assert+ ~@forms))

;
; -<> and -<>> macros
; SWISS ARROW
; see https://github.com/rplevy/swiss-arrows
;

(defmacro -<>
  "the 'diamond wand': top-level insertion of x in place of single
   positional '<>' symbol within the threaded form if present, otherwise
   mostly behave as the thread-first macro. Also works with hash literals
   and vectors.

   SEE: https://github.com/rplevy/swiss-arrows"
  [& forms]
  `(sa/-<> ~@forms))

(defmacro -<>>
  "the 'diamond spear': top-level insertion of x in place of single
   positional '<>' symbol within the threaded form if present, otherwise
   mostly behave as the thread-last macro. Also works with hash literals
   and vectors.

   SEE: https://github.com/rplevy/swiss-arrows"
  [& forms]
  `(sa/-<>> ~@forms))

(tests
  (-<> (range 10)
       (filter even? <>)
       (map str <>)
       vec
       (conj "X")
       str/join
       (take 3 <>)
       (map long <>)
       (reduce + <>)
       [:before <> :after]) := [:before 150 :after]

  (-<>> (range 10)
        (filter even?)
        (map str)
        vec
        (conj <> "X")
        str/join
        (take 3)
        (map long)
        (reduce +)
        [:before <> :after]) := [:before 150 :after]

  :end-test)

(defmacro not-yet
  "Throws an exception: 'not yet implemented'."
  []
  `(throw (ex-info (str "not yet implemented exception") {})))

(defn boom
  "throw an not-yet-implemented exception"
  [something]
  (not-yet))

(tests
  (expect-ex (boom nil)) := clojure.lang.ExceptionInfo
  :end-test)


(defn is-subpath?
  "Checks if path1 is a subsequence of path2 using loop and recur."
  [path1 path2]
  (if (or (nil? path1)
          (empty? path1))
    true
    (let [subs-of-2 (partition (count path1) 1 path2)
          s1        (seq path1)]
      (boolean (some true? (map #(= % s1) subs-of-2))))))


(tests
  (is-subpath? [30 5] [10 2 3 30 5]) := true
  (is-subpath? [10 5] [10 2 3 30 5]) := false
  (is-subpath? [] [10 2 3 30 5]) := true
  (is-subpath? [30 5] [30 1 5]) := false
  (is-subpath? [30 5] [30 1 30 5]) := true)


(defn remove-subpaths
  "Removes subpaths from the collection of paths."
  [paths]
  (filter (fn [path]
            (not-any? #(and (not= path %)
                            (is-subpath? path %))
                      paths))
          paths))

(tests
  (deflet
    (def paths [[10 2 3 30 5] [4 4 4] [30 5] [10 2 3] [5] [10 2 3 30]])
    (remove-subpaths paths) := [[10 2 3 30 5] [4 4 4]])

  :end-tests)