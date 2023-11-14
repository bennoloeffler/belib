(ns belib.http-parallel
  (:require [clojure.core.async :refer [go go-loop <!! <! >! chan close! thread alts! alts!! timeout]]))



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



(defn n-api-fake-fns
  "Creates n api-fake fns that can be called without parameters."
  [n] (mapv #(fn [] (api-fake (str %"-text"))) (range n)))

(comment
  (n-api-fake-fns 50))


(defn fn-in-chan
  "Run a fn-call in a go-block and return a channel with the result."
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

(comment
  (let [a-fn #(+ 40 2)        ; a fn that returns 42
        ch   (fn-in-chan a-fn); get the chan
        val  (<!! ch)]        ; get the val
    val) ;should be 42

  nil)

(defn timeout-channel
  "try to get value from v-chan. If it takes longer than
   timeout-ms, return {:exception :user-timeout :data {:ms timeout-ms}}."
  [v-chan timeout-ms]
  (let [t-chan (timeout timeout-ms)
        [result ch] (alts!! [t-chan v-chan])]
    (if (= ch t-chan)
      {:exception :user-timeout :data {:ms timeout-ms}}
      result)))

(defn fn-timeout
  ""
  [fn-call timeout-ms]
  (let [v-chan (fn-in-chan fn-call)]
    (timeout-channel v-chan timeout-ms)))

(comment
  (fn-timeout #(Thread/sleep 1000)  500)
  (fn-timeout #(Thread/sleep 100) 500) ;nil exception
  ;(fn-timeout #((Thread/sleep 100) :result) 500)
  (fn-timeout #(do (Thread/sleep 100) :result) 500)
  (fn-timeout (fn [](Thread/sleep 100) :result) 500))



(defn chans-of-fns
  "Creates n channels with the results of the
  functions in vector fns."
  [fns]
  (mapv #(fn-in-chan %) fns))

(comment
  (chans-of-fns (n-api-fake-fns 50)))

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

  (time (results-of-fns (n-api-fake-fns 550) 700))

  nil)





