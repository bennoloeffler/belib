(ns belib.email                                  ; SEE https://github.com/drewr/postal
  (:require [postal.core :refer [send-message]]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.core.async :as async :refer [go <! timeout]]))


;; belib-host
;; belib-user
;; belib-pass
;; belib-port (default 587)
;; belib-tls (default true)
(defn config-from-env [env] {:host (or (:belib-mail-host env) nil); "mail.gmx.net"
                             :user (or (:belib-mail-user env) nil); "benno.loeffler@gmx.de"
                             :pass (or (:belib-mail-pass env) nil)
                             :port (or (:belib-mail-port env) 587)
                             :tls  (or (:belib-mail-tls  env) true)})


(defn send-mail! [body-html body-text subject receiver cfg]
  (let [mail-data   {:from    "benno.loeffler@gmx.de";"decision-konsent@gmx.de"
                     :to      receiver
                     :subject subject
                     :body    [:alternative
                               {:type    "text/plain"
                                :content body-text}
                               {:type    "text/html"
                                :content body-html}]}
        send-result (send-message cfg mail-data)]))
;(println "sent: " mail-data)
;(println "result: " send-result)))

(comment
  (def result (send-mail! "<html><head> </head><body><h1>Heading 1</h1><p>This is a test.</p></body></html>"
                          "Notfall-Text" "eine Testmail" "loeffler@v-und-s.de"))
  (println result))


(defn big-random [] (str (* 1N
                            (rand-int Integer/MAX_VALUE)
                            (rand-int Integer/MAX_VALUE)
                            (rand-int Integer/MAX_VALUE)
                            (rand-int Integer/MAX_VALUE))))


(defn create-invitation [email server-name server-port konsent-id]
  (let [invitation-id  (big-random)
        invitation-url (str "http://"
                            server-name
                            (if (contains? #{"80" 80 nil ""} server-port)
                              ""
                              (str ":" server-port))
                            "/api/konsent/invitation/"
                            invitation-id "/"
                            konsent-id)]
    {:invitation-url invitation-url
     :invitation-id  invitation-id
     :guest-email    email}))

(comment (create-invitation "bel@loef.de" "konsent-app.com" 88 27)
         (create-invitation "bel@loef.de" "konsent-app.com" nil 27))

(defn create-invitations [konsent server-name server-port]
  (let [participants  (-> konsent :konsent :participants)
        invitations-v (map
                        (fn [email]
                          (let [invitation (create-invitation email
                                                              server-name
                                                              server-port
                                                              (:id konsent))]
                            [(keyword (:invitation-id invitation)) invitation])) participants)
        invitations   (into {} invitations-v)]
    (assoc-in konsent [:konsent :invitations] invitations)))

(comment (create-invitations {:id 17 :konsent {:participants ["bel@soso" "karl@immutant.de"]}}
                             "dec-kon.com" 8080))

(defn check-email [email]
  (clojure.string/includes? email "@"))

(defn send-invitation! [invitation konsent]
  ;(println "invitation: " invitation)
  (if (check-email (:guest-email invitation))
    (send-mail! (str "<html><head> </head><body><h1>Konsent: " (-> konsent :konsent :short-name)
                     "</h1><p>You have been invited to that konsent by: " (-> konsent :konsent :owner)
                     "</p><p>Click link to participate as guest:<br>" (:invitation-url invitation)
                     "</p><p>Long description:<br>"
                     "</p><p>" (-> konsent :konsent :problem-statement) "</p></body></html>")
                "Please enable html to see the invitation!"
                "Please contribute to decision"
                (:guest-email invitation))
    (println "WARNING: ignoring email: " (:guest-email invitation))))

(comment (send-invitation! (create-invitation "benno.loeffler@gmx.de" "konsent-app.com" nil 27) "hugo.h@gmx.de"))

(defn send-invitations! [konsent]
  ;(println "konsent: " konsent)
  ;(println "invitations: " (-> konsent :konsent :invitations vals))
  (let [owner      (-> konsent :konsent :owner)
        iterations (-> konsent :konsent :invitations vals)]
    (go (loop [i (first iterations)
               r (next iterations)]
          (when i (try
                    (send-invitation! i konsent)
                    (println "invitation-email sent to: " (:guest-email i))
                    (<! (timeout 1000))
                    (catch Throwable t
                      (println "INVITATION FAILED:")
                      (pprint i)
                      (println "DUE TO EXCEPTION:")
                      (clojure.stacktrace/print-cause-trace t))))
          (when r (recur (first r) (next r)))))))



(comment (send-invitations! {:id      41,
                             :konsent {:invitations       {:955280395323058556552030487589495680 {:guest-email    "loeffler@v-und-s.de",
                                                                                                  :invitation-id  "955280395323058556552030487589495680",
                                                                                                  :invitation-url "http://localhost:3000/invitation/955280395323058556552030487589495680/41"}},
                                       :iterations        [{:allowed-proposers ["loeffler@v-und-s.de"
                                                                                "benno.loeffler@gmx.de"],
                                                            :discussion        [],
                                                            :q&a               [],
                                                            :ready             [],
                                                            :votes             []}],
                                       :owner             "benno.loeffler@gmx.de",
                                       :participants      ["loeffler@v-und-s.de"],
                                       :problem-statement "pb",
                                       :short-name        "sn",
                                       :timestamp         1624995708783}}))