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
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.set :as set]))
(t/refer-tupelo)

; #todo Should have clojure.core/any?
; #todo    =>  spec/anything
; #todo    =>  spec/pass-all (vs. spec/pass-none)
; #todo    =>  spec/dont-care
; #todo    =>  (constantly true)
; #todo    =>  #(fn [& _] true)
; #todo    =>  clojure.core/->true (vs. clojure.core/->false)
; #todo    =>  clojure.core/true-fn (vs. clojure.core/false-fn)

; #todo    =>  (s/def ::s/anything (constantly true))
; #todo    =>  (s/def ::s/nothing (constantly false))

;   (s/fdef clojure.core/declare
;       :args (s/cat :names (s/* simple-symbol?))
;       :ret any?)   ; #todo conflicts with clojure.core/not-any?
(dotest
  (s/def ::s/anything (constantly true))
  (s/def ::s/nothing (constantly false))
  (is (s/valid? ::s/anything 5 ))
  (is (s/valid? ::s/anything "joe" ))
  (is (s/valid? ::s/anything { :blah 42 :blue 66 :hut! 'hut! }))
  (isnt (s/valid? ::s/nothing 5 )))

;-----------------------------------------------------------------------------
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

(dotest
  (s/def :event/type keyword?)
  (s/def :event/timestamp int?)
  (s/def :search/url string?)
  (s/def :error/message string?)
  (s/def :error/code int?)

  (defmulti event-type :event/type)
  (defmethod event-type :event/search [_]
    (s/keys :req [:event/type :event/timestamp :search/url]))
  (defmethod event-type :event/error [_]
    (s/keys :req [:event/type :event/timestamp :error/message :error/code]))

  (s/def :event/event (s/multi-spec event-type :event/type))

  (is (s/valid? :event/event {:event/type      :event/search
                              :event/timestamp 1463970123000
                              :search/url      "https://clojure.org"}))
  (is (s/valid? :event/event {:event/type      :event/error
                              :event/timestamp 1463970123000
                              :error/message   "Invalid host"
                              :error/code      500}))

  (is= (s/explain-data :event/event {:event/type :event/restart})
    #:clojure.spec.alpha{:problems [{:path   [:event/restart],
                                     :pred   'tst.tupelo.x.spec/event-type,
                                     :val    #:event{:type :event/restart},
                                     :reason "no method",
                                     :via    [:event/event],
                                     :in     []}],
                         :spec     :event/event,
                         :value    #:event{:type :event/restart}})

  (is= (s/conform (s/coll-of keyword?) [:a :b :c]) [:a :b :c])
  (is= (s/conform (s/coll-of number?) [1 2 3]) [1 2 3])

  (s/def ::vnum3 (s/coll-of number? :kind vector? :count 3 :distinct true :into #{}))
  (is= (s/conform ::vnum3 [1 2 3]) #{1 2 3})
  (is= (s/conform ::vnum3 #{1 2 3})  :clojure.spec.alpha/invalid )
  (is= (s/conform ::vnum3 [1 1 1])  :clojure.spec.alpha/invalid )
  (is= (s/conform ::vnum3 [1 2 :a])  :clojure.spec.alpha/invalid )

  (s/def ::point (s/tuple double? double? double? ))
  (is= (s/conform ::point [1.5 2.0 3.1]) [1.5 2.0 3.1] )

  (s/def ::scores (s/map-of string? int?))
  (is= (s/conform ::scores{"Sally" 1000 "joe" 500})  {"Sally" 1000 "joe" 500}))

(dotest
  (s/def ::ingredient (s/cat :quantity number?  :unit keyword? ))
  (is= (s/conform ::ingredient [2 :teaspoon]) {:quantity 2, :unit :teaspoon} )

  (s/def ::seq-of-keywords (s/* keyword?))
  (is= (s/conform ::seq-of-keywords [:a :b :c])  [:a :b :c])

  (s/def ::odds-then-maybe-even (s/cat :odds (s/+ odd?)
                                        :even (s/? even?)))
  (is (s/valid? ::odds-then-maybe-even [1 3 5 66]))
  (is (s/valid? ::odds-then-maybe-even [1 3 5]))
  (isnt (s/valid? ::odds-then-maybe-even [1 3 66 5]))
  (isnt (s/valid? ::odds-then-maybe-even [2]))

  ; opts are alternating pairs of keyword & boolean
  (s/def ::opts (s/* (s/cat :opt keyword? :val boolean?)))
  (is= (s/conform ::opts [:silent false :verbose true])
    [{:opt :silent, :val false} {:opt :verbose, :val true}] )

  (s/def ::config (s/* (s/cat :prop string?
                         :val (s/alt :s string? :b boolean?))))
  (is= (s/conform ::config ["-server" "foo" "-verbose" true "-user" "joe"])
    [{:prop "-server", :val [:s "foo"]}
     {:prop "-verbose", :val [:b true]}
     {:prop "-user", :val [:s "joe"]}])

  (is= (s/describe ::seq-of-keywords)
    '(* keyword?))
  (is= (s/describe ::odds-then-maybe-even)
    '(cat :odds (+ odd?) :even (? even?)))
  (is= (s/describe ::opts)
    '(* (cat :opt keyword? :val boolean?)))

  (s/def ::even-strings (s/& (s/* string?) #(even? (count %))))
  (is (s/valid? ::even-strings ["a" "b"]))
  (is (s/valid? ::even-strings ["a" "b" "c" "d"]))
  (isnt (s/valid? ::even-strings ["a"]))
  (isnt (s/valid? ::even-strings ["a" "b" "c"]))

  (s/def ::nested
  (s/cat :names-kw #{:names}
         :names (s/spec (s/* string?))
         :nums-kw #{:nums}
         :nums (s/spec (s/* number?))))
  (is= (s/conform ::nested [:names ["a" "b"] :nums [1 2 3]])
    {:names-kw :names, :names ["a" "b"], :nums-kw :nums, :nums [1 2 3]} )

  (s/def ::unnested
    (s/cat :names-kw #{:names}
      :names (s/* string?)
      :nums-kw #{:nums}
      :nums (s/* number?)))
  (is= (s/conform ::unnested [:names "a" "b" :nums 1 2 3])
    {:names-kw :names, :names ["a" "b"], :nums-kw :nums, :nums [1 2 3]}) )

(dotest
  (def suit? #{:club :diamond :heart :spade}) ; remember sets are predicates
  (def rank? (into #{:jack :queen :king :ace} (thru 2 10))) ; remember sets are predicates
  (def deck (forv [suit suit? rank rank?]
              [rank suit]))
  (is (set/subset? #{[2 :club] [5 :diamond] [:queen :heart]}
        (into #{} deck)))

  (s/def ::card (s/tuple rank? suit?))
  (s/def ::hand (s/* ::card))

  (s/def ::name string?)
  (s/def ::score int?)
  (s/def ::player (s/keys :req [::name ::score ::hand]))

  (s/def ::players (s/* ::player))
  (s/def ::deck (s/* ::card))
  (s/def ::game (s/keys :req [::players ::deck]))

  (def kenny {::name  "Kenny Rogers"
              ::score 100
              ::hand  []})
  (is (s/valid? ::player kenny))
  (is (s/valid? ::player {::name  "Kenny Rogers"
                          ::score 100
                          ::hand  [[2 :heart]]}))
  (isnt (s/valid? ::player {::name  "Kenny Rogers"
                            ::score 100
                            ::hand  [[2 :hurts]]}))

  ; Create a generator for ::player, generate a single sample value, and verify it is valid
  (is (s/valid? ::player (gen/generate (s/gen ::player)))))



