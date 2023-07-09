(ns belib.macro)

;;------------------------------
;; https://learnxinyminutes.com/docs/clojure-macros/
;;------------------------------
(defmacro make-fn
  "Create a function from a macro.
  Example:
  (apply (make-fn and) '(true true false true))"
  [m]
  `(fn [& args#]
     (eval
       (cons '~m args#))))

