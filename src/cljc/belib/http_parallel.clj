(ns belib.http-parallel
  (:require
    [clojure.core.async :refer [go go-loop <!! <! >! chan close! thread alts! alts!! timeout]]
    [hyperfiddle.rcf :refer [tests]]
    [clj-http.client :as client]
    [belib.core :refer [vec-remove-elem]])
  (:import
    [clojure.core.async.impl.channels ManyToManyChannel]))

(System/setProperty "clojure.core.async.pool-size" "500")

(defn chan?
  "is an object a core.async channel?
   ManyToManyChannel"
  [obj]
  (instance? ManyToManyChannel obj))

(tests
  (chan? (chan)) := true
  (chan? :no-chan) := false
  :end-tests)

;;
;; call http api in parallel with core.async
;;

;;
;; If you need more: set the degree of parallelism.
;; CAUTION: Don't overuse! It will bring down
;; the JVM or the OS.
;; READ: https://www.baeldung.com/jvm-max-threads
;    (System/setProperty "clojure.core.async.pool-size" "500")
; or may be done at system start:
;    java -Dclojure.core.async.pool-size=10 -jar my-app.jar

(defn api-fake
  "API call that sleeps 200 to 400ms.
   10% of calls fail...
   90% of calls return a vector of doubles."
  [text]
  ;(println "calling api-fake: " text)
  (Thread/sleep (+ 200 (rand-int 200)))
  (let [fail (< (rand) 0.1)]
    (if fail
      (throw (ex-info "api-emb-fake failed" {:text text}))
      (let [nums   (map int text)
            num    (count nums)
            max    (apply max nums)
            normed (map #(double (/ % max)) nums)
            result (->> (repeat (inc (quot 100 num)) normed)
                        flatten
                        (take 5)
                        vec)]
        {:param text
         :result result}))))

(tests
  (api-fake "abcd12345")
  := {:param "abcd12345", :result [0.97 0.98 0.99 1.0 0.49]}

  :end-test)


(defn n-api-fake-fns
  "Creates n api-fake fns that can be called without parameters."
  [n] (mapv #(fn [] (api-fake (str %"-text"))) (range n)))

(comment
  (n-api-fake-fns 50))


(defn fn-in-chan
  "Run a fn-call in a go-block and return a channel with the result.
  if (:exception (fn-in-chan ...)) is nil, the call was successful."
  [fn-call]
  (let [result-chan (chan)]
    (go (>! result-chan
            (try (let [;_ (print "S")
                       ;_ (flush)
                       result (fn-call)
                       result (if (nil? result)
                                {:exception :got-nil-from-fn-for-channel}
                                result)]
                       ;_ (print "E")
                       ;_ (flush)]
                   result)
                 (catch Exception e {:exception (ex-message e)
                                     :data (ex-data e)}))))
    result-chan))

(tests

  "normal call"
  (let [a-fn #(+ 40 2)        ; a fn that returns 42
        ch   (fn-in-chan a-fn); wrap in a channel
        val  (<!! ch)]        ; get the val
    val) := 42

  "exception"
  (let [a-fn #(throw (ex-info "test" {:data "test"})) ; a fn that throws
        ch   (fn-in-chan a-fn); wrap in a channel
        val  (<!! ch)]
    val) := {:exception "test", :data {:data "test"}}

  "nil to channel"
  (let [a-fn #(seq []) ; a fn that returns nil
        ch   (fn-in-chan a-fn); wrap in a channel
        val  (<!! ch)]
    val) := {:exception :got-nil-from-fn-for-channel}

  :end-tests)



(defn timeout-channel
  "Try to get value from v-chan. If it takes longer than
   timeout-ms, return {:exception :user-timeout :data {:ms timeout-ms}}."
  [v-chan timeout-ms]
  (let [t-chan (timeout timeout-ms)
        [result ch] (alts!! [t-chan v-chan])]
    (if (= ch t-chan)
      {:exception :user-timeout :data {:ms timeout-ms}}
      result)))

(defn fn-timeout
  "Try to get value from fn-call. If the fn-call takes longer than
   timeout-ms, return {:exception :user-timeout :data {:ms timeout-ms}}."
  [fn-call timeout-ms]
  (let [v-chan (fn-in-chan fn-call)]
    (timeout-channel v-chan timeout-ms)))

(tests
  (fn-timeout #(Thread/sleep 1000)  500)
  := {:exception :user-timeout, :data {:ms 500}}

  (fn-timeout #(Thread/sleep 100) 500) ; returns nil to channel
  := {:exception :got-nil-from-fn-for-channel}

  "java.lang.NullPointerException when calling (nil ...)"
  (fn-timeout #((println)) 500)
  := {:exception nil, :data nil}

  (fn-timeout #(do (Thread/sleep 100) :result) 500)
  := :result

  (fn-timeout (fn [](Thread/sleep 100) :result) 500)
  := :result

  :end-tests)


(defn chans-of-fns
  "Creates channels with the results of the
  functions in vector fns."
  [fns]
  (mapv #(fn-in-chan %) fns))

(tests
  (map chan? (chans-of-fns (n-api-fake-fns 5)))
  := [true true true true true]
  :end-tests)

(defn results-of-chans
  "Returns a vector of the results of the channels.
   Each channel may only contain ONE result!
   Channels may not be closed."
  [chans timeout-ms]
  (let [size (count chans)
        t-chan (timeout timeout-ms)]
    (loop [i 0
           results []]
      (if (= i size)
        results
        (let [[result ch] (alts!! (conj chans t-chan))]
          (if (= ch t-chan)
            (recur size
                   {:user-timeout {:ms timeout-ms}})
            (recur (inc i)
                   (conj results result))))))))

(defn results-of-fns
  "Calls a vector of fns in parallel. Depending on the
   number of threads in the threadpool, this can be
   MUCH faster than calling the fns sequentially.
   Thread-Pool default size = processors + 2
   Setting it higher, up to 500...
   E.g.:
   (System/setProperty \"clojure.core.async.pool-size\" \"50\")"
  [fns-vec timeout-ms]
  (-> fns-vec ;; vector of (fn [] (api-call par1 par2 ...)
      chans-of-fns ;; vector of channels, each with a result of an api-call
      (results-of-chans timeout-ms))) ;; vector of vals of channels



;(System/setProperty "clojure.core.async.pool-size" "500")
(System/setProperty "clojure.core.async.pool-size" "10")

(comment
  (System/setProperty "clojure.core.async.pool-size" "50")
  (System/setProperty "clojure.core.async.pool-size" "500")
  ;; Threadpool to 50. Then:
  ;; 4 seconds for 550 calls that take between 200 and 400 ms each
  ;; sequentially, it would be...
  ;; try it. Set it to 1, reload jvm. Then run the code below.
  ;; (System/setProperty "clojure.core.async.pool-size" "1")
  ;; 166 seconds!
  (-> (n-api-fake-fns 550) ;; vector of (fn [] (api-call par1 par2 ...)
      chans-of-fns ;; vector of channels, each with a result of an api-call
      (results-of-chans 500) ;; vector of vals of channels
      first
      time)

  (api-fake "abcd12345")
  (time (results-of-fns (n-api-fake-fns 550) 700))

  nil)

;;
;; USE ASYNC HTTP API
;;

;;
;; call http api in parallel with core.async
;;

(defn api-to-channel
  "Call an url per get and return a channel immediately.
   The result is put into the channel when the response
   is received."
  [url]
  (let [result-ch (chan)]
    (client/get url
                {:async? true}
                ;; respond callback
                (fn [response] (go (>! result-ch response)
                                   (close! result-ch)))
                ;; raise callback
                (fn [exception] (go (>! result-ch {:exception (.getMessage exception)
                                                   :data exception})
                                    (close! result-ch))))
    result-ch))


(defn results-of-chns
  "Returns a vector of the results of the channels.
   Each channel may only contain ONE result!
   Channels should be closed."
  [channels timeout-ms]
  (let [t-ch (timeout timeout-ms)]
    (loop [results []
           channels channels]
      (if (seq channels)
        (let [[result ch] (alts!! (conj channels t-ch))]
          (if (= ch t-ch)
            [:timeout]
            (recur (conj results result)
                   (vec-remove-elem ch channels))))
        results))))

(defn chans-n
  "create n channels with a dummy api call to mocky.io"
  [n]
  (->> #(api-to-channel "https://run.mocky.io/v3/0ad167a9-dcb3-4aff-902b-85d229751a4a")
       (repeat n)))

(tests

  (:body (<!! (api-to-channel "https://run.mocky.io/v3/0ad167a9-dcb3-4aff-902b-85d229751a4a")))
  := ""


  (count (results-of-chns
           (mapv
             #(%)
             (chans-n 5))
           2000))
  := 5

  :end-tests)



