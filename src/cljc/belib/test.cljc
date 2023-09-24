(ns belib.test
  #?(:clj
     (:require [net.cgrand.macrovich :as macros])
     :cljs
     (:require-macros [net.cgrand.macrovich :as macros]
       #_[belib.test :refer [expect-ex]]))

  #?(:clj (:import [java.lang Throwable])))


(defmacro expect-ex
  "check for exceptions in rcf tests:

  :clj
  (tests
    (expect-ex (/ 1 0)) := ArithmeticException)

  :cljs
  (tests
    (expect-ex (/ 1 0)) := js/XXX )

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
  "check for exceptions in rcf tests
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
  "check for exceptions in rcf tests
  and returns it. So you can check details.

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

