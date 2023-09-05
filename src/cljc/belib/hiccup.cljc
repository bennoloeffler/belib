(ns belib.hiccup
  (:require [clojure.string :as str]))

(defn cs
  "Join classes together.
  This functions may be called with expressions.
  Instead of [:i.fas.fa-1x.fa-user-plus
  you may call [:i.fas.fa-user-plus (cs (if (< @size 2) \"fa-1x\" \"fa-3x\"))"
  [& names]
  {:class (str/join " " (filter identity names))})



