;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.x.spec
  (:use tupelo.test )
  (:require
    [tupelo.core :as t]
    [tupelo.impl :as i]
    [clojure.spec.alpha :as s]))
(t/refer-tupelo)


(dotest
  (is= (s/conform even? 4) 4)
  (is= (s/conform even? 5) :clojure.spec.alpha/invalid)

  (is= (s/valid? even? 4) true)  ; NOTE: normally just use (is   (s/valid? ...))
  (is= (s/valid? even? 5) false) ; NOTE: normally just use (isnt (s/valid? ...))

  (is (s/valid? nil? nil))
  (is (s/valid? string? "abc"))
  (is   (s/valid? #(< % 5) 3))
  (isnt (s/valid? #(< % 5) 9))

  (is (s/valid? inst? (java.util.Date.)))

  (is   (s/valid? #{:club :diamond :heart :spade} :club ))
  (isnt (s/valid? #{:club :diamond :heart :spade} 42 ))
  (is   (s/valid? #{42 43 44}  42))
)

(dotest
  (s/def ::date inst?)
  (s/def ::suit #{:club :diamond :heart :spade})
  (is (s/valid? ::date (java.util.Date.)))
  (is (s/valid? ::suit :club))

  (s/def ::big-even (s/and int? even? #(< 1000 %)))
  (is (s/valid? ::big-even 2222))
  (isnt (s/valid? ::big-even 10 ))
  (isnt (s/valid? ::big-even :foo))

  (s/def ::name-or-id (s/or :name string?
                            :id   int?))
  (is   (s/valid? ::name-or-id  "abc"))
  (is   (s/valid? ::name-or-id  100))
  (isnt (s/valid? ::name-or-id  :foo))

  (is= (s/conform ::name-or-id "abc") [:name "abc"] )
  (is= (s/conform ::name-or-id 100) [:id 100])

  (isnt (s/valid? string? nil))
  (is   (s/valid? (s/nilable string?) nil))
  (is= (s/explain-data ::name-or-id :foo)
      #:clojure.spec.alpha{:problems
                          '({:path [:name], ; ### NOTE need to quote list ***
                             :pred clojure.core/string?,
                             :val :foo,
                             :via [:tst.tupelo.x.spec/name-or-id],
                             :in []}
                            {:path [:id],
                             :pred clojure.core/int?,
                             :val :foo,
                             :via [:tst.tupelo.x.spec/name-or-id],
                             :in []}),
                           :spec :tst.tupelo.x.spec/name-or-id,
                           :value :foo} ) )

(dotest
  (def email-regex  #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
  (s/def ::email-type (s/and string? #(re-matches email-regex %)))
  (s/def ::acctid int?)
  (s/def ::first-name string?)
  (s/def ::last-name string?)
  (s/def ::email ::email-type)
  (s/def ::person (s/keys :req [::first-name ::last-name ::email]
                          :opt [::phone] ))

  (is (s/valid? ::person {::first-name "Elon"
                          ::last-name  "Musk"
                          ::email      "elon@example.com"} ))
  (isnt (s/valid? ::person {::first-name "Elon" } ))
  (isnt (s/valid? ::person {::first-name "Elon"
                            ::last-name  "Musk"
                            ::email      "n/a"} ))

  (s/def :unq/person (s/keys :req-un [::first-name ::last-name ::email]
                             :opt-un [::phone] ))
  (is= (s/conform :unq/person {:first-name "Elon"
                               :last-name  "Musk"
                               :email      "elon@example.com"})
    {:first-name "Elon", :last-name "Musk", :email "elon@example.com"})

  (defrecord Person [first-name last-name email phone])
  (is (val= (s/conform :unq/person (->Person "Elon" "Musk" "elon@example.com" nil)))
    {:first-name "Elon", :last-name "Musk", :email "elon@example.com", :phone nil} ) )

(dotest
  (s/def ::port number?)
  (s/def ::host string?)
  (s/def ::id keyword?)
  (s/def ::server (s/keys* :req [::id ::host] :opt [::port]))
  (is= (s/conform ::server [::id :s1 ::host "example.com" ::port 5555])
    #:tst.tupelo.x.spec{:id :s1, :host "example.com", :port 5555} ) )

(dotest
  (s/def :animal/kind  string?)
  (s/def :animal/says string?  )
  (s/def :animal/common (s/keys :req [:animal/kind :animal/says])  ) ; common keys for any animal
  (s/def :dog/tail? boolean? )
  (s/def :dog/breed string? )
  (s/def :animal/dog (s/merge :animal/common
                       (s/keys :req [:dog/tail? :dog/breed]))) ; merge in addl keys to create dog spec

  (is (s/valid? :animal/dog {:animal/kind "dog"
                             :animal/says "woof"
                             :dog/tail?   true
                             :dog/breed   "retriever"})))


