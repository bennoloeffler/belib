(ns belib.malli
  (:require
    #_[puget.printer :refer [cprint]]
    [malli.core :as m]
    [clojure.pprint :as pp :refer [pprint]]
    [malli.error :as me]
    [malli.util :as mu]
    [malli.dev.pretty :as pretty]
    #_[malli.experimental :as mx]
    #_[malli.dev :as dev]
    [hyperfiddle.rcf :refer [tests]]
    [clojure.string :as str]))


(hyperfiddle.rcf/enable! true)

;;
;; little helper...
;;
(defn hum-err
  "return very short message of a data problem
  regarding the given schema.
  Default is to close all maps.
  Use :DONT-CLOSE as last parameter to prevent closing."
  [schema data & [dont-close]]
  (let [close-if (fn [schema] (if dont-close
                                schema
                                (mu/closed-schema schema)))]
    (-> schema
        (close-if)
        #_(malli.dev.pretty/explain data)
        (m/explain data)
        (me/with-spell-checking)
        (me/humanize))))

(tests

  ; tuple error --> message
  (hum-err [:tuple :int :string :boolean]
           [1 "23" :false])
  := [nil nil ["should be a boolean"]]

  ; no error --> nil
  (hum-err [:tuple :int :string :boolean]
           [1 "23" false])
  := nil

  ; CLOSING IS THE DEFAULT
  ; spelling and additional keys are checked.
  (hum-err [:map
            [:age :int]
            [:street :string]]
           {:age 18 :streets "dr" :name "Carl"})
  := {:streets ["should be spelled :street"],
      :name    ["disallowed key"]}

  ; OPEN MAPS can be asked for by :DONT-CLOSE
  (hum-err [:map
            [:age :int]
            [:street :string]]
           {:street "n4"
            :age    18
            :add    :because-not-closed-we-can-add-additional-keys}
           ; YOU MAY PREVENT CLOSING LIKE THAT
           :DONT-CLOSE)
  := nil

  nil)


(defn hum-err-test [schema data & [dont-close]]
  ;; if nil --> return true
  ;; if message --> return false
  ;; make it usable for tests
  (not (hum-err schema data dont-close)))

(tests

  (hum-err-test
    [:tuple :int :string :boolean]
    [1 "23" :false]) := false
  (hum-err-test [:tuple :int :string :boolean]
                [1 "23" false]) := true

  (hum-err-test [:map
                 [:age :int]
                 [:street :string]]
                {:street "n4"
                 :age    18
                 :add    :because-not-closed}
                ; YOU MAY PREVENT CLOSING LIKE THAT
                :DONT-CLOSE) := true

  (hum-err-test [:map
                 [:age :int]
                 [:street :string]]
                {:street "n4"
                 :age    18
                 ; CLOSED by DEFAULT
                 :add    :although-closed}) := false)




(defn hum-err-test-pr [schema data & [dont-close]]
  (let [close-if (fn [schema] (if dont-close
                                schema
                                (-> schema
                                    (mu/closed-schema))))]

    (not (-> schema
             close-if
             (pretty/explain data)
             (me/with-spell-checking)))))


(comment ;; -pr is just to debug...
  (tests
    (hum-err-test-pr [:tuple :int :string :boolean]
                     [1 "23" :false])
    := true

    (hum-err-test-pr [:tuple :int :string :boolean]
                     [1 "23" false])
    := true

    (hum-err-test-pr [:map
                      [:age :int]
                      [:street :string]]
                     {:streets "n4"
                      :age     18
                      ; CLOSED by DEFAULT
                      :add     :although-closed})
    := false

    (hum-err-test-pr [:map
                      [:age :int]
                      [:street :string]]
                     {:street "n4"
                      :age    18
                      ; CLOSED by DEFAULT
                      :add    :although-closed}
                     :DONT-CLOSE)
    := true))



(defn hum-err-mult
  "little helper to test
  malli schemas against data and
  see the result of a complete
  data set against one schema.
  For better readability:
  require [puget.printer :refer [cprint]]
  and use it with hum-err-mult.
  Schemas are CLOSED before they are used!
  E.g.
  (cprint
    (hum-err-mult
      [:vector :int]

      [2 3 4]
      [1 :wrong 3]
      []
      [1 (count [:x :y])])"
  [schema & datas]
  (vec (map-indexed
         (fn [idx data] (let [msg (hum-err schema data)]
                          (if msg
                            [(str (inc idx) "-failed:") :data data :msg msg]
                            [(keyword (str (inc idx) "-passed."))])))
         datas)))


(comment
  (hum-err-mult
    [:vector :int]

    [2 3 4]
    [1 :wrong 3]
    []
    [1 (count [:x :y])]))


(defn hum-err-mult-test
  "little helper for automated testing
  malli schemas against data and
  compare every dataset against true or false.
  Schemas are CLOSED before they are tested!
  E.g.
    failing my be expected with:
    :no :fail false nil
    passing my be signalled with everything else, but
    :yes :pass would be good to read.
  (tests
    (hum-err-mult-test
      [:vector :int]

      :pass [2 3 4]
      :fail [1 :wrong 3]
      :yes  []
      :no   ['a-symbol]) := true)"
  [schema & datas]
  (let [results (vec (map-indexed
                       (fn [idx [expected-to-pass data]] (let [msg              (hum-err schema data)
                                                               expected-to-pass (not (contains? #{:no :fail false nil} expected-to-pass))]
                                                           (cond
                                                             (and expected-to-pass (not msg)) nil
                                                             (and (not expected-to-pass) msg) nil
                                                             (and expected-to-pass msg)
                                                             [(str (inc idx) "-should-pass-but-failed:") :data data :msg msg]
                                                             (and (not expected-to-pass) (not msg))
                                                             [(str (inc idx) "-should-fail-but-passed:") :data data :msg msg])))
                       (partition 2 datas)))
        errors  (remove nil? results)]
    (if (-> errors count pos?)
      (println errors)
      true)))

(tests
  (hum-err-mult-test
    [:vector :int]

    :pass [2 3 4]
    :fail [1 :wrong 3]
    :pass []
    :pass [1 (count [:x :y])]) := true)

(defn hum-err-mult-test-pr
  "just to print all the failing ones..."
  [schema & datas]
  (let [results       (vec (map-indexed
                             (fn [idx [expected-to-pass data]] (let [msg              (hum-err schema data)
                                                                     expected-to-pass (not (contains? #{:no :fail false nil} expected-to-pass))]
                                                                 (cond
                                                                   (and expected-to-pass (not msg)) [:no-pr :no-err (str (inc idx) "-should-pass-and-passed:")]
                                                                   (and (not expected-to-pass) msg) [:pr :no-err (str (inc idx) "-should-fail-and-failed:") :data data :msg msg]
                                                                   (and expected-to-pass msg)
                                                                   [:pr :err (str (inc idx) "-should-pass-but-failed:") :data data :msg msg]
                                                                   (and (not expected-to-pass) (not msg))
                                                                   [:no-pr :err (str (inc idx) "-should-fail-but-passed:") :data data :msg msg])))
                             (partition 2 datas)))
        rm-indicators (fn [sub-results] (map #(drop 2 %) #_(fn [[_ _ idx-str data-key data msg-key msg]
                                                                [idx-str data-key data msg-key msg]])
                                             sub-results))

        errors        (rm-indicators (remove #(= :no-err (second %)) results))
        fail-messages (rm-indicators (remove #(= :no-pr (first %)) results))]

    (if (-> fail-messages count pos?)
      (do (println "\nall FAILED MESSAGES of m/explain:")
          (pprint fail-messages))
      (println "\nNO FAILED MESSAGES."))
    (if (-> errors count pos?)
      (do
        (println "\nall TEST ERRORS - did not fullfill expected :pass or :fail")
        (pprint errors))
      (do (println "NO TEST FAILD.")
          true))))


(tests
  (str/includes? (with-out-str (hum-err-mult-test-pr
                                 [:vector :int]

                                 :pass [2 3 4]
                                 :fail [1 :wrong 3]
                                 :pass []
                                 :pass [1 (count [:x :y])]))
                 "ERROR") := false)

(tests
  (str/includes? (with-out-str (hum-err-mult-test-pr
                                 [:vector :int]
                                 :pass []
                                 :pass [1 :wrong 3]
                                 :fail [1 2 3]
                                 :fail [1 :wrong 3]))
                 "ERROR") := true)


(comment
  (pos? 1)
  (partition 2 '(3 4 6 3 2))
  (not (contains? #{:no :fail false nil} :yes)))

(def letters "abcdefghijklmnopqrstuvwxyz")
(def set-of-lowercase-letters (set (seq letters)))
(def set-of-uppercase-letters (set (seq (clojure.string/upper-case letters))))
(def digits "0123456789")
(def set-of-digits (set (seq digits)))
(def specials "!#$%&/()=?_-.,;'´`")
(def set-of-specials (set (seq specials)))
(def re-whitespace #"\s")

(def password-schema
  [:and
   [:string {:min 8 :max 40}]
   [:fn
    {:error/message (str "should contain at least one number like: " digits)}
    (fn [data] (seq (clojure.set/intersection (set (seq data)) set-of-digits)))]
   [:fn
    {:error/message (str "should contain at least one special character: " specials)}
    (fn [data] (seq (clojure.set/intersection (set (seq data)) set-of-specials)))]
   [:fn
    {:error/message "should contain uppercase and lowercase letters, e.g. Ab..."}
    (fn [data] (and (seq (clojure.set/intersection (set (seq data)) set-of-uppercase-letters))
                    (seq (clojure.set/intersection (set (seq data)) set-of-lowercase-letters))))]
   [:fn
    {:error/message (str "should not contain whitespace characters like return, space, tab, etc.")}
    (fn [data] (not (re-find re-whitespace data)))]])

(comment
  (re-find re-whitespace "dat da"))

(tests
  "check for invalid password"

  (hum-err password-schema "toos") := ["should be between 8 and 40 characters"
                                       "should contain at least one number like: 0123456789"
                                       "should contain at least one special character: !#$%&/()=?_-.,;'´`"
                                       "should contain uppercase and lowercase letters, e.g. Ab..."]

  (hum-err password-schema "too-S") := ["should be between 8 and 40 characters"
                                        "should contain at least one number like: 0123456789"]

  (hum-err password-schema "t0o-S") := ["should be between 8 and 40 characters"]

  (hum-err password-schema "t0o-S..........................................................")
  := ["should be between 8 and 40 characters"]

  (hum-err password-schema "t0o-s.rtrtrt") := ["should contain uppercase and lowercase letters, e.g. Ab..."]
  (hum-err password-schema "T0O-S.RTRTRT") := ["should contain uppercase and lowercase letters, e.g. Ab..."]

  "check for valid password"

  (hum-err password-schema "Ok&1.......") := nil

  (hum-err password-schema "Ok&1. ......")
  := ["should not contain whitespace characters like return, space, tab, etc."]


  :end-tests)


(comment
  (seq (clojure.set/intersection (set (seq "1234")) #{\1 \0}))
  (seq (clojure.set/intersection (set (seq "1234")) #{\0})))

(def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
(defn email? [s]
  (re-matches email-regex s))

(def email-schema
  [:fn
   {:error/message "should be an email like: hugo@somthing.com "}
   (fn [data] (email? data))])

(tests
  (hum-err email-schema "notAnEmail") := ["should be an email like: hugo@somthing.com "]
  (hum-err email-schema "is-an@email.com") := nil
  :end-tests)

