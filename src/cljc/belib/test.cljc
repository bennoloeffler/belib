(ns belib.test
  (:require
    [hyperfiddle.rcf :refer [tests]]
    #_#?(:clj
         (:require [net.cgrand.macrovich :as macros])
         :cljs
         (:require-macros [net.cgrand.macrovich :as macros])))
  ;#?(:cljs (:require-macros [belib.test :refer [expect-ex]])) ;
  (:import
    #?(:clj [java.lang Throwable])))

;(hyperfiddle.rcf/enable! true)

;; THE PROBLEM: cant use :throws from hyperfiddle.rfc
;; see belib.test-test

(defmacro expect-ex
  "check for exceptions in rcf tests,
  since :throws does not work.

  :clj
  (tests
    (expect-ex (/ 1 0)) := ArithmeticException
    (expect-ex ArithmeticException (/ 1 0)) := true)

  :cljs
  (tests
    (expect-ex (boom)) := js/Error )

  You need:
   [com.hyperfiddle/rcf \"20220405\"] in project.clj
   [hyperfiddle.rcf :refer [tests]] required and
   (hyperfiddle.rcf/enable! true)"
  ([expr]
   (if (:ns &env) ;; :ns only exists in CLJS
     `(try ~expr
           (catch js/Error e# (type e#)))
     `(try ~expr
           (catch Throwable e# (type e#)))))
  ([exception-type expr]
   (if (:ns &env) ;; :ns only exists in CLJS
     `(try (do
             ~expr
             false)
           (catch js/Error e#
             (instance? ~exception-type e#)))
     `(try (do
             ~expr
             false)
           (catch Throwable e#
             (instance? ~exception-type e#))))))


(defmacro return-ex
  "Check for exceptions in rcf tests
  and returns it. So you can check details.

    #?(:cljs (ex-message (return-ex (throw (js/Error. \"something\")))) := \"something\"
       :clj  (ex-message (return-ex (throw (RuntimeException. \"something else\")))) := \"something else\"))

  You need:
   [com.hyperfiddle/rcf \"20220405\"] in project.clj
   [hyperfiddle.rcf :refer [tests]] required and
   (hyperfiddle.rcf/enable! true)"
  [expr]
  (if (:ns &env) ;; :ns only exists in CLJS
    `(try ~expr
          (catch js/Error e# e#))
    `(try ~expr
          (catch Throwable e# e#))))


(defmacro return-error-kw-if-ex
  "Check for exceptions in rcf tests
  and returns :error if one get's thrown.
  So you can't check details.

  :clj and :cljs
  (tests
    (return-error-kw-if-ex (/ 1 0)) := :error)

  You need:
   [com.hyperfiddle/rcf \"20220405\"] in project.clj
   [hyperfiddle.rcf :refer [tests]] required and
   (hyperfiddle.rcf/enable! true)"
  [expr]
  (if (:ns &env) ;; :ns only exists in CLJS
    `(try ~expr
          (catch js/Error e# :error))
    `(try ~expr
          (catch Throwable e# :error))))
