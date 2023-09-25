(ns belib.core
  (:require [tick.core :as t]
            [hyperfiddle.rcf :refer [tests]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [time-literals.read-write]))

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

(def bpp #?(:cljs cljs.pprint/pprint
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








