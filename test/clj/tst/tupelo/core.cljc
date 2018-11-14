;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.core
  #?@(:clj [
  (:use tupelo.dev tupelo.test )
            (:require
              [clojure.string :as str]
              [tupelo.core :as t] ; #todo finish migration to (:use tupelo.core)
              [tupelo.core :as i]
              [tupelo.string :as ts]
              [tupelo.string :as tstr]) ]) )

; (s/instrument-all)
; (s/instrument #'tupelo.core/truthy?)  ; instrument just one var

;-----------------------------------------------------------------------------
; Java version stuff

#?(:clj (do

(defn fn-any [] 42)
(defn fn7 [] (t/if-java-1-7-plus
               7
               (throw (ex-info "Unimplemented prior to Java 1.7: " nil))))
(defn fn8 [] (t/if-java-1-8-plus
               8
               (throw (ex-info "Unimplemented prior to Java 1.8: " nil))))

(dotest
  (when (is-java-1-7?)
    (throws? (fn8)))

  (when (is-java-1-8-plus?)
    (is= 8 (fn8)))

  (is= 7 (fn7))
  (is= 42 (fn-any))

  (with-redefs [java-version (constantly "1.7")]
    (is   (t/java-version-min? "1.7"))
    (isnt (t/java-version-min? "1.7.0"))
    (isnt (t/java-version-min? "1.7.0-b1234"))
    (isnt (t/java-version-min? "1.8"))

    (is   (t/java-version-matches? "1.7"))
    (isnt (t/java-version-matches? "1.7.0"))
    (isnt (t/java-version-matches? "1.7.0-b1234"))
    (isnt (t/java-version-matches? "1.8"))
    )
  (with-redefs [java-version (constantly "1.7.0")]
    (is   (t/java-version-min? "1.7"))
    (is   (t/java-version-min? "1.7.0"))
    (isnt (t/java-version-min? "1.7.0-b1234"))
    (isnt (t/java-version-min? "1.8"))

    (is   (t/java-version-matches? "1.7"))
    (is   (t/java-version-matches? "1.7.0"))
    (isnt (t/java-version-matches? "1.7.0-b1234"))
    (isnt (t/java-version-matches? "1.8"))
    )
  (with-redefs [java-version (constantly "1.7.0-b1234")]
    (is   (t/java-version-min? "1.7"))
    (is   (t/java-version-min? "1.7.0"))
    (is   (t/java-version-min? "1.7.0-b1234"))
    (isnt (t/java-version-min? "1.8"))

    (is   (t/java-version-matches? "1.7"))
    (is   (t/java-version-matches? "1.7.0"))
    (is   (t/java-version-matches? "1.7.0-b1234"))
    (isnt (t/java-version-matches? "1.8"))
    )

  (with-redefs [java-version (constantly "1.7") ]
    (when false
      (println "testing java 1.7")
      (t/spyx (t/is-java-1-7?))
      (t/spyx (t/is-java-1-8?))
      (t/spyx (t/is-java-1-7-plus?))
      (t/spyx (t/is-java-1-8-plus?)))

    (is   (t/is-java-1-7?))
    (is   (t/is-java-1-7-plus?))
    (isnt (t/is-java-1-8?))
    (isnt (t/is-java-1-8-plus?)) )

  (with-redefs [java-version (constantly "1.8") ]
    (when false
      (println "testing java 1.8")
      (t/spyx (t/is-java-1-7?))
      (t/spyx (t/is-java-1-8?))
      (t/spyx (t/is-java-1-7-plus?))
      (t/spyx (t/is-java-1-8-plus?)))

    (isnt (t/is-java-1-7?))
    (is   (t/is-java-1-7-plus?))
    (is   (t/is-java-1-8?))
    (is   (t/is-java-1-8-plus?)) ) )

;-----------------------------------------------------------------------------
; Clojure version stuff

(dotest
  (binding [*clojure-version* {:major 1 :minor 7}]
    (is   (i/is-clojure-1-7-plus?))
    (isnt (i/is-clojure-1-8-plus?))
    (isnt (i/is-clojure-1-9-plus?))
    (is   (i/is-pre-clojure-1-8?))
    (is   (i/is-pre-clojure-1-9?)))
  (binding [*clojure-version* {:major 1 :minor 8}]
    (is   (i/is-clojure-1-7-plus?))
    (is   (i/is-clojure-1-8-plus?))
    (isnt (i/is-clojure-1-9-plus?))
    (isnt (i/is-pre-clojure-1-8?))
    (is   (i/is-pre-clojure-1-9?)))
  (binding [*clojure-version* {:major 1 :minor 9}]
    (is   (i/is-clojure-1-7-plus?))
    (is   (i/is-clojure-1-8-plus?))
    (is   (i/is-clojure-1-9-plus?))
    (isnt (i/is-pre-clojure-1-8?))
    (isnt (i/is-pre-clojure-1-9?)))
)

(dotest
  (let [val1 (into (sorted-map) {:a 1 :b 2})]
    (is= "val1 => <#clojure.lang.PersistentTreeMap {:a 1, :b 2}>"
      (ts/collapse-whitespace (with-out-str (t/spyxx val1))))
    (is= "(+ 2 3) => <#java.lang.Long 5>"
      (ts/collapse-whitespace (with-out-str (t/spyxx (+ 2 3)))))
    (is= "(mapv inc (range 3)) => <#clojure.lang.PersistentVector [1 2 3]>"
      (ts/collapse-whitespace (with-out-str (t/spyxx (mapv inc (range 3)))))) ))

;(sp/def ::vector (sp/coll-of clj/any :kind vector?))
;(dotest
;  (is   (sp/valid? ::vector [1 2 3]))
;  (isnt (sp/valid? ::vector '(1 2 3)))
;  (isnt (sp/valid? ::vector {:a 1}))
; ;(spyx (sp/exercise ::vector))
;)

#_(tst/defspec ^:slow t-keep-if-drop-if 999
  (prop/for-all [vv (gen/vector gen/int) ]
    (let [even-1      (keep-if   even?  vv)
          even-2      (drop-if   odd?   vv)
          even-filt   (filter    even?  vv)

          odd-1       (keep-if   odd?   vv)
          odd-2       (drop-if   even?  vv)
          odd-rem     (remove    even?  vv) ]
      (and  (= even-1 even-2 even-filt)
            (=  odd-1  odd-2  odd-rem)))))

#_(tst/defspec ^:slow t-keep-if-drop-if-set 999
  (prop/for-all [ss (gen/set gen/int) ]
    (let [even-1      (keep-if   even?  ss)
          even-2      (drop-if   odd?   ss)
          even-filt   (into #{} (filter even? (seq ss)))

          odd-1       (keep-if   odd?   ss)
          odd-2       (drop-if   even?  ss)
          odd-rem     (into #{} (remove even? (seq ss))) ]
      (and  (= even-1 even-2 even-filt)
            (=  odd-1  odd-2  odd-rem)))))

#_(tst/defspec ^:slow t-keep-if-drop-if-map-key 99  ; seems to hang if (< 99 limit)
  (prop/for-all [mm (gen/map gen/int gen/keyword {:max-elements 99} ) ]
    (let [even-1      (keep-if  (fn [k v] (even? k))  mm)
          even-2      (drop-if  (fn [k v] (odd?  k))  mm)
          even-filt   (into {} (filter #(even? (key %)) (seq mm)))

          odd-1      (keep-if  (fn [k v] (odd?  k))  mm)
          odd-2      (drop-if  (fn [k v] (even? k))  mm)
          odd-rem    (into {} (remove #(even? (key %)) (seq mm)))
    ]
      (and  (= even-1 even-2 even-filt)
            (=  odd-1  odd-2  odd-rem)))))

#_(tst/defspec ^:slow t-keep-if-drop-if-map-value 99  ; seems to hang if (< 99 limit)
  (prop/for-all [mm (gen/map gen/keyword gen/int {:max-elements 99} ) ]
    (let [even-1      (keep-if  (fn [k v] (even? v))  mm)
          even-2      (drop-if  (fn [k v] (odd?  v))  mm)
          even-filt   (into {} (filter #(even? (val %)) (seq mm)))

          odd-1      (keep-if  (fn [k v] (odd?  v))  mm)
          odd-2      (drop-if  (fn [k v] (even? v))  mm)
          odd-rem    (into {} (remove #(even? (val %)) (seq mm)))
    ]
      (and  (= even-1 even-2 even-filt)
            (=  odd-1  odd-2  odd-rem)))))



; #todo ***** toptop *****



(dotest
  (testing "single string"
    (is (= ""         (clip-str 0 "abcdefg")))
    (is (= "a"        (clip-str 1 "abcdefg")))
    (is (= "ab"       (clip-str 2 "abcdefg")))
    (is (= "abc"      (clip-str 3 "abcdefg")))
    (is (= "abcd"     (clip-str 4 "abcdefg")))
    (is (= "abcde"    (clip-str 5 "abcdefg"))))
  (testing "two strings"
    (is (= ""         (clip-str 0 "abc defg")))
    (is (= "a"        (clip-str 1 "abc defg")))
    (is (= "ab"       (clip-str 2 "abc defg")))
    (is (= "abc"      (clip-str 3 "abc defg")))
    (is (= "abc "     (clip-str 4 "abc defg")))
    (is (= "abc d"    (clip-str 5 "abc defg"))))
  (testing "two strings & char"
    (is (= ""         (clip-str 0 "ab" \c "defg")))
    (is (= "a"        (clip-str 1 "ab" \c "defg")))
    (is (= "ab"       (clip-str 2 "ab" \c "defg")))
    (is (= "abc"      (clip-str 3 "ab" \c "defg")))
    (is (= "abcd"     (clip-str 4 "ab" \c "defg")))
    (is (= "abcde"    (clip-str 5 "ab" \c "defg"))))
  (testing "two strings & digit"
    (is (= ""         (clip-str 0 "ab" 9 "defg")))
    (is (= "a"        (clip-str 1 "ab" 9 "defg")))
    (is (= "ab"       (clip-str 2 "ab" 9 "defg")))
    (is (= "ab9"      (clip-str 3 "ab" 9 "defg")))
    (is (= "ab9d"     (clip-str 4 "ab" 9 "defg")))
    (is (= "ab9de"    (clip-str 5 "ab" 9 "defg"))))
  (testing "vector"
    (is (= ""               (clip-str  0 [1 2 3 4 5] )))
    (is (= "["              (clip-str  1 [1 2 3 4 5] )))
    (is (= "[1"             (clip-str  2 [1 2 3 4 5] )))
    (is (= "[1 2"           (clip-str  4 [1 2 3 4 5] )))
    (is (= "[1 2 3 4"       (clip-str  8 [1 2 3 4 5] )))
    (is (= "[1 2 3 4 5]"    (clip-str 16 [1 2 3 4 5] ))))
  (testing "map"
    (is (= ""               (clip-str  0 (sorted-map :a 1 :b 2) )))
    (is (= "{"              (clip-str  1 (sorted-map :a 1 :b 2) )))
    (is (= "{:"             (clip-str  2 (sorted-map :a 1 :b 2) )))
    (is (= "{:a "           (clip-str  4 (sorted-map :a 1 :b 2) )))
    (is (= "{:a 1, :"       (clip-str  8 (sorted-map :a 1 :b 2) )))
    (is (= "{:a 1, :b 2}"   (clip-str 16 (sorted-map :a 1 :b 2) ))))
  (testing "set"
    (let [tst-set (sorted-set 5 4 3 2 1) ]
      (is (= ""             (clip-str  0 tst-set )))
      (is (= "#"            (clip-str  1 tst-set )))
      (is (= "#{"           (clip-str  2 tst-set )))
      (is (= "#{1 "         (clip-str  4 tst-set )))
      (is (= "#{1 2 3 "     (clip-str  8 tst-set )))
      (is (= "#{1 2 3 4 5}" (clip-str 16 tst-set )))))
)

(dotest
  (is (= [\a ]              (chars-thru \a \a)))
  (is (= [\a \b]            (chars-thru \a \b)))
  (is (= [\a \b \c]         (chars-thru \a \c)))

  (is (= [\a ]              (chars-thru 97 97)))
  (is (= [\a \b]            (chars-thru 97 98)))
  (is (= [\a \b \c]         (chars-thru 97 99)))

  (throws? Exception (chars-thru 987654321 987654321))
  (throws? Exception (chars-thru \c \a))
  (throws? Exception (chars-thru 99 98)))

(dotest
  (is= [] (drop-at (range 1) 0))

  (is= [  1] (drop-at (range 2) 0))
  (is= [0  ] (drop-at (range 2) 1))

  (is= [  1 2] (drop-at (range 3) 0))
  (is= [0   2] (drop-at (range 3) 1))
  (is= [0 1  ] (drop-at (range 3) 2))

  (throws? IllegalArgumentException (drop-at []         0))
  (throws? IllegalArgumentException (drop-at (range 3) -1))
  (throws? IllegalArgumentException (drop-at (range 3)  3)))

(dotest
  (is= [9] (insert-at [] 0 9))

  (is= [9 0] (insert-at [0] 0 9))
  (is= [0 9] (insert-at [0] 1 9))

  (is= [9 0 1] (insert-at [0 1] 0 9))
  (is= [0 9 1] (insert-at [0 1] 1 9))
  (is= [0 1 9] (insert-at [0 1] 2 9))

  (throws? IllegalArgumentException (insert-at [] -1 9))
  (throws? IllegalArgumentException (insert-at []  1 9))

  (throws? IllegalArgumentException (insert-at [0] -1 9))
  (throws? IllegalArgumentException (insert-at [0]  2 9))

  (throws? IllegalArgumentException (insert-at [0 1] -1 9))
  (throws? IllegalArgumentException (insert-at [0 1]  3 9)))

(dotest
  (is= [9] (replace-at (range 1) 0 9))

  (is= [9 1] (replace-at (range 2) 0 9))
  (is= [0 9] (replace-at (range 2) 1 9))

  (is= [9 1 2] (replace-at (range 3) 0 9))
  (is= [0 9 2] (replace-at (range 3) 1 9))
  (is= [0 1 9] (replace-at (range 3) 2 9))

  (throws? IllegalArgumentException (replace-at []         0 9))
  (throws? IllegalArgumentException (replace-at (range 3) -1 9))
  (throws? IllegalArgumentException (replace-at (range 3)  3 9)))

(dotest             ; #todo need more tests
  (is= (mapv #(mod % 3) (thru -6 6)) [0 1 2 0 1 2 0 1 2 0 1 2 0])
  (is= (mapv #(idx [0 1 2] %) (thru -3 3)) [0 1 2 0 1 2 0]))

; #todo add different lengths a/b
; #todo add missing entries a/b
(dotest
  (is      (matches?  []    [] ))
  (is      (matches?  [1]   [1] ))
  (isnt    (matches?  [1]   [2] ))
  ;        (matches?  [1]   [1 2] )))  ***** error *****
  (is      (matches?  [_]   [1] ))
  (is      (matches?  [_]   [nil] ))
  (is      (matches?  [_]   [1] [2] [3]))
  (is      (matches?  [1 2] [1 2] ))
  (is      (matches?  [_ 2] [1 2] ))
  (is      (matches?  [1 _] [1 2] ))
  (is      (matches?  [1 _] [1 2] [1 3] [1 nil] ))
  (is      (matches?  [1 _ 3] [1 2 3] [1 nil 3] ))

  (is      (matches?  {:a 1} {:a 1} ))
  (isnt    (matches?  {:a 1} {:a 2} ))
  (isnt    (matches?  {:a 1} {:b 1} ))
  (is      (matches?  {:a _} {:a 1} {:a 2} {:a 3} ))
  ;        (matches?  { _ 1} {:a 1} )   ***** error *****

  (is      (matches?  {:a _ :b _       :c 3}
                      {:a 1 :b [1 2 3] :c 3} ))
  (isnt    (matches?  {:a _ :b _       :c 4}
                      {:a 1 :b [1 2 3] :c 3} ))
  (isnt    (matches?  {:a _ :b _       :c 3}
                      {:a 1 :b [1 2 3] :c 4} ))
  (isnt    (matches?  {:a 9 :b _       :c 3}
                      {:a 1 :b [1 2 3] :c 3} ))

  (is      (matches?  {:a _ :b _       :c 3}
                      {:a 1 :b [1 2 3] :c 3}
                      {:a 2 :b 99      :c 3}
                      {:a 3 :b nil     :c 3} ))
  (isnt    (matches?  {:a _ :b _       :c 3}
                      {:a 1 :b [1 2 3] :c 9}
                      {:a 2 :b 99      :c 3}
                      {:a 3 :b nil     :c 3} ))
  (isnt    (matches?  {:a _ :b _       :c 3}
                        {:a 1 :b [1 2 3] :c 3}
                        {:a 2 :b 99      :c 3}
                        {:a 3 :b nil     :c 9} ))
)

; #todo add different lengths a/b
; #todo add missing entries a/b
(dotest
  (testing "vectors"
    (is   (wild-match? [1]  [1] ))
    (is   (wild-match? [1]  [1] [1] ))
    (is   (wild-match? [:*] [1] [1] ))
    (is   (wild-match? [:*] [1] [9] ))

    (is   (wild-match? [1] [1] ))
    (is   (wild-match? [1] [1] [1] ))

    (isnt (wild-match? [1] [ ] ))
    (isnt (wild-match? [ ] [1] ))
    (isnt (wild-match? [1] [ ] [ ] ))
    (isnt (wild-match? [ ] [1] [ ] ))
    (isnt (wild-match? [ ] [ ] [1] ))
    (isnt (wild-match? [1] [1] [ ] ))
    (isnt (wild-match? [1] [ ] [1] ))

    (is   (wild-match? [1 2  3]
                       [1 2  3] ))
    (is   (wild-match? [1 :* 3]
                       [1 2  3] ))
    (is   (wild-match? [1 :* 3]
                       [1 2  3]
                       [1 9  3] ))
    (isnt (wild-match? [1 2  3]
                       [1 2  9] ))
    (isnt (wild-match? [1 2   ]
                       [1 2  9] ))
    (isnt (wild-match? [1 2  3]
                       [1 2   ] ))

    (is   (wild-match? [1  [2 3]]
                       [1  [2 3]] ))
    (is   (wild-match? [:* [2 3]]
                       [1  [2 3]] ))
    (is   (wild-match? [:* [2 3]]
                       [1  [2 3]]
                       [9  [2 3]] ))
    (is   (wild-match? [1  [2 :*]]
                       [1  [2 33]]
                       [1  [2 99]] ))
    (is   (wild-match? [1  :*]
                       [1   2]
                       [1  [2 3]] ))
    (isnt (wild-match? [1  [2 3]]
                       [1  [2 9]] ))
  )
  (testing "maps"
    (is (wild-match? {:a 1 } {:a 1} ))
    (is (wild-match? {:a :*} {:a 1} ))
    (is (wild-match? {:a :*} {:a 1 } {:a 1 } ))
    (is (wild-match? {:a :*} {:a 1 } {:a 9 } ))
    (is (wild-match? {:a :*} {:a :*} {:a 9 } ))
    (is (wild-match? {:a :*} {:a :*} {:a :*} ))

    (isnt (wild-match? {:a 1 } {:a 9} ))
    (isnt (wild-match? {:a 1 } {:a 1 :b 2} ))
    (isnt (wild-match? {:a :*} {:b 1} ))
    (isnt (wild-match? {:a :*} {:a 1} {:b 1} ))
    (isnt (wild-match? {:a :*} {:a 1 :b 2} ))

    (let [vv {:a 1  :b {:c 3}}
          tt {:a 1  :b {:c 3}}
          w2 {:a :* :b {:c 3}}
          w5 {:a 1  :b {:c :*}}
          zz {:a 2  :b {:c 3}}
    ]
      (is   (wild-match? tt vv))
      (is   (wild-match? w2 vv))
      (is   (wild-match? w5 vv))
      (isnt (wild-match? zz vv)))
  )
  (testing "vecs & maps 1"
    (let [vv [:a 1  :b {:c  3} ]
          tt [:a 1  :b {:c  3} ]
          w1 [:* 1  :b {:c  3} ]
          w2 [:a :* :b {:c  3} ]
          w3 [:a 1  :* {:c  3} ]
          w5 [:a 1  :b {:c :*} ]
          zz [:a 2  :b {:c  3} ]
    ]
      (is (wild-match? tt vv))
      (is (wild-match? w1 vv))
      (is (wild-match? w2 vv))
      (is (wild-match? w3 vv))
      (is (wild-match? w5 vv))
      (isnt (wild-match? zz vv)))
  )
  (testing "vecs & maps 2"
    (let [vv {:a 1  :b [:c  3] }
          tt {:a 1  :b [:c  3] }
          w2 {:a :* :b [:c  3] }
          w4 {:a 1  :b [:*  3] }
          w5 {:a 1  :b [:c :*] }
          z1 {:a 2  :b [:c  3] }
          z2 {:a 1  :b [:c  9] }
    ]
      (is (wild-match? tt vv))
      (is (wild-match? w2 vv))
      (is (wild-match? w4 vv))
      (is (wild-match? w5 vv))
      (isnt (wild-match? z1 vv))
      (isnt (wild-match? z2 vv)))
  )
  (testing "sets"
    (is   (wild-match? #{1} #{1} ))
    (isnt (wild-match? #{1} #{9} ))
    (isnt (wild-match? #{1} #{:a :b} ))
    (is   (wild-match? #{1  #{:a :b}}
                       #{1  #{:a :b} }))
    (isnt (wild-match? #{1  #{:a :c}}
                       #{1  #{:a :x} }))
  ))

(defrecord SampleRec [a b])
(dotest
  (let [sr1 (->SampleRec 1 2)]
    (is (map? sr1))
    (is (val= sr1 {:a 1 :b 2}))
    (is (val= 1 1))
    (is (val= "abc" "abc"))
    (is (val= [1 2 3] [1 2 3]))
    (is (val= #{1 2 sr1} #{1 2 {:a 1 :b 2}}))
    (is (val= [1 2 3 #{1 2 sr1}] [1 2 3 #{1 2 {:a 1 :b 2}}])) ) )

(dotest
  (isnt (wild-match? #{1 2} #{1 2 3 4}))
  (isnt (wild-match? {:pattern #{1 2}
                      :values  [#{1 2 3 4}]}))
  (is (wild-match? {:subset-ok true
                    :pattern   #{1 2}
                    :values    [#{1 2 3 4}]}))

  (isnt (wild-match? {:a 1} {:a 1 :b 2}))
  (isnt (wild-match? {:pattern {:a 1}
                      :values  [{:a 1 :b 2}]}))
  (is (wild-match? {:submap-ok true
                    :pattern   {:a 1}
                    :values    [{:a 1 :b 2}]}))

  (isnt (wild-match? '(1 2) '(1 2 3 4)))
  (isnt (wild-match? {:pattern '(1 2)
                      :values  ['(1 2 3 4)]}))
  (is (wild-match? {:subvec-ok true
                    :pattern   '(1 2)
                    :values    ['(1 2 3 4)]}))

  (isnt (wild-match? [1 2] [1 2 3 4]))
  (isnt (wild-match? {:pattern [1 2]
                      :values  [[1 2 3 4]]}))
  (is (wild-match? {:subvec-ok true
                    :pattern   [1 2]
                    :values    [[1 2 3 4]]}))

  (isnt (wild-submatch? #{1 :*}    #{1 2 3 4}))
  (is (wild-submatch?   #{1 2}     #{1 2 3 4}))
  (is (wild-submatch?    {:a :*}    {:a 1 :b 2}))
  (is (wild-submatch?   '(1 :* 3)  '(1 2 3 4)))
  (is (wild-submatch?    [1 :* 3]   [1 2 3 4]))

  (is (submatch? #{1 2} #{1 2 3 4}))
  (is (submatch? {:a 1} {:a 1 :b 2}))
  (is (submatch? '(1 2) '(1 2 3 4)))
  (is (submatch? [1 2 3] [1 2 3 4]))
  (isnt (submatch? [1 :* 3] [1 2 3 4]))
  (isnt (submatch? {:a :*} {:a 1 :b 2}))
  (isnt (submatch? #{1 :*} #{1 2 3 4}))

  (let [sample-rec (->SampleRec 1 2)]
    (isnt= sample-rec {:a 1 :b 2})
    (is (wild-submatch? sample-rec {:a 1 :b 2}))
    (is (wild-submatch? {:a 1 :b 2} sample-rec))
    (is (submatch? sample-rec {:a 1 :b 2}))
    (is (submatch? {:a 1 :b 2} sample-rec))))

(dotest
  (is (i/set-match? #{1 2 3} #{1 2 3}))
  (is (i/set-match? #{:* 2 3} #{1 2 3}))
  (is (i/set-match? #{1 :* 3} #{1 2 3}))
  (is (i/set-match? #{1 2 :*} #{1 2 3}))

  (is (i/set-match? #{1 2 3 4 5} #{1 2 3 4 5}))
  (is (i/set-match? #{:* 2 3 4 5} #{1 2 3 4 5}))
  (is (i/set-match? #{1 :* 3 4 5} #{1 2 3 4 5}))
  (is (i/set-match? #{1 2 :* 4 5} #{1 2 3 4 5}))
  (is (i/set-match? #{1 2 3 :* 5} #{1 2 3 4 5}))
  (is (i/set-match? #{1 2 3 4 :*} #{1 2 3 4 5}))

  (is   (wild-item? :*))
  (isnt (wild-item? :a))

  (is   (wild-item? :*))
  (isnt (wild-item? :a))
  (isnt (wild-item? 5))
  (isnt (wild-item? "hello"))

  (is   (wild-item? [:* 2 3]))
  (is   (wild-item? [1 [:* 3]]))
  (is   (wild-item? [1 [2 [:*]]]))
  (isnt (wild-item? [1 2 3]))
  (isnt (wild-item? [1 [2 3]]))
  (isnt (wild-item? [1 [2 [3]]]))

  (is   (wild-item? #{:* 2 3}))
  (is   (wild-item? #{1 #{:* 3}}))
  (is   (wild-item? #{1 #{2 #{:*}}}))
  (isnt (wild-item? #{1 2 3}))
  (isnt (wild-item? #{1 #{2 3}}))
  (isnt (wild-item? #{1 #{2 #{3}}}))

  (is   (wild-item? {:* 1 :b 2 :c 3}))
  (is   (wild-item? {:a {:* 2 :c 3}}))
  (is   (wild-item? {:a {:b {:* 3}}}))
  (is   (wild-item? {:a :* :b 2 :c 3}))
  (is   (wild-item? {:a {:b :* :c 3}}))
  (is   (wild-item? {:a {:b {:c :*}}}))
  (isnt (wild-item? {:a 1 :b 2 :c 3}))
  (isnt (wild-item? {:a {:b 2 :c 3}}))
  (isnt (wild-item? {:a {:b {:c 3}}}))

  (is (i/set-match? #{#{1 2 3} #{4 5 :*}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{1 2 3} #{4 :* 6}} #{#{1 2 3} #{4 5  6}}))
  (is (i/set-match? #{#{1 2 3} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{:* 2 3} #{4 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{1 :* 3} #{4 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{1 2 :*} #{4 5 6}} #{#{1 2 3} #{4 5 6}}))

  (is (i/set-match? #{#{1 :* 3} #{4 5 :*}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{1 2 :*} #{4 :* 6}} #{#{1 2 3} #{4 5  6}}))
  (is (i/set-match? #{#{:* 2 3} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{:* 2 3} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{1 :* 3} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{1 2 :*} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
)

(dotest
  (is= (range 10)   ; vector/list
    (unnest  0 1 2 3 4 5 6 7 8 9 )
    (unnest  0 1 2 [3 [4] 5 6] 7 [8 9])
    (unnest [0 1 2 3 4 5 6 7 8 9])
    (unnest [0 [1 2 3 4 5 6 7 8 9]])
    (unnest [0 [1 [2 3 4 5 6 7 8 9]]])
    (unnest [0 [1 [2 [3 4 5 6 7 8 9]]]])
    (unnest [0 [1 [2 [3 [4 5 6 7 8 9]]]]])
    (unnest [0 [1 [2 [3 [4 [5 6 7 8 9]]]]]])
    (unnest [0 [1 [2 [3 [4 [5 [6 7 8 9]]]]]]])
    (unnest [0 [1 [2 [3 [4 [5 [6 [7 8 9]]]]]]]])
    (unnest [0 [1 [2 [3 [4 [5 [6 [7 [8 9]]]]]]]]])
    (unnest [0 [1 [2 [3 [4 [5 [6 [7 [8 [9]]]]]]]]]])
    (unnest [[[[[[[[[[0] 1] 2] 3] 4] 5] 6] 7] 8] 9])
    (unnest [0 1 [2 [3 [4] 5] 6] 7 8 9]) )

  (is= [1 2 3 4 5] (unnest [[[1] 2] [3 [4 [5]]]]))

  (is= (set [:a :1 :b 2 :c 3]) ; map
    (set (unnest [:a :1 {:b 2 :c 3}]))
    (set (unnest [:a :1 {[:b] 2 #{3} :c}]))
    (set (unnest [:a :1 {:b 2 :c 3}]))
    (set (unnest [:a :1 {:b {:c [2 3]}}])))
  (is= #{ 1 2 3 4 5 6 } (set (unnest {1 {2 {3 {4 {5 6}}}}})))

  (is= (set (range 10)) ; set
    (set (unnest #{0 1 2 3 4 5 6 7 8 9}))
    (set (unnest #{0 1 #{2 3 4 5 6 7 8} 9}))
    (set (unnest #{0 1 #{2 3 #{4 5 6} 7 8} 9})) )
  (is= #{ 1 2 3 4 5 6 } (set (unnest #{1 #{2 #{3 #{4 #{5 #{6}}}}}}))))

(dotest
  (is   (starts-with? (range 0 3) (range 0 0)))

  (is   (starts-with? (range 0 3) (range 0 1)))
  (is   (starts-with? (range 0 3) (range 0 2)))
  (is   (starts-with? (range 0 3) (range 0 3)))

  (isnt (starts-with? (range 0 3) (range 1 2)))
  (isnt (starts-with? (range 0 3) (range 1 3)))

  (isnt (starts-with? (range 0 3) (range 2 3)))

  (isnt (starts-with? (range 1 3) (range 0 1)))
  (isnt (starts-with? (range 1 3) (range 0 2)))
  (isnt (starts-with? (range 1 3) (range 0 3)))

  (is   (starts-with? (range 1 3) (range 1 2)))
  (is   (starts-with? (range 1 3) (range 1 3)))

  (isnt (starts-with? (range 1 3) (range 2 3)))

  (isnt (starts-with? (range 2 3) (range 0 1)))
  (isnt (starts-with? (range 2 3) (range 0 2)))
  (isnt (starts-with? (range 2 3) (range 0 3)))

  (isnt (starts-with? (range 2 3) (range 1 2)))
  (isnt (starts-with? (range 2 3) (range 1 3)))

  (is   (starts-with? (range 2 3) (range 2 3)))

  (isnt (starts-with? (range 3 3) (range 0 1)))
  (isnt (starts-with? (range 3 3) (range 0 2)))
  (isnt (starts-with? (range 3 3) (range 0 3)))
  )


(dotest
  ; (println "t-throws enter")
  ; (newline) (spyx (throws?-impl '(/ 1 2)))
  ; (newline) (spyx (throws?-impl 'Exception '(/ 1 2)))
  ; (newline) (spyx (throws?-impl 'ArithmeticException '(/ 1 2)))

  (throws? (/ 1 0))
  (throws? Exception (/ 1 0))             ; catches specified Throwable (or subclass)
  (throws? ArithmeticException (/ 1 0))   ; catches specified Throwable (or subclass)

  ; (println "t-throws exit")
)

(dotest
  (testing "fibo stuff"
    (is= (take  0 (fibonacci-seq))  [] )
    (is= (take  5 (fibonacci-seq))  [0 1 1 2 3] )
    (is= (take 10 (fibonacci-seq))  [0 1 1 2 3 5 8 13 21 34] )

    (is= (fibo-thru  0) [0] )
    (is= (fibo-thru  1) [0 1 1] )
    (is= (fibo-thru  2) [0 1 1 2] )
    (is= (fibo-thru  3) [0 1 1 2 3] )
    (is= (fibo-thru  4) [0 1 1 2 3] )
    (is= (fibo-thru  5) [0 1 1 2 3 5] )
    (is= (fibo-thru  6) [0 1 1 2 3 5] )
    (is= (fibo-thru  7) [0 1 1 2 3 5] )
    (is= (fibo-thru  8) [0 1 1 2 3 5 8] )
    (is= (fibo-thru 34) [0 1 1 2 3 5 8 13 21 34] )

    (is=  0 (fibo-nth 0))
    (is=  1 (fibo-nth 1))
    (is=  1 (fibo-nth 2))
    (is=  2 (fibo-nth 3))
    (is=  3 (fibo-nth 4))
    (is=  5 (fibo-nth 5))
    (is=  8 (fibo-nth 6))
    (is= 13 (fibo-nth 7))
    (is= 21 (fibo-nth 8))
    (is= 34 (fibo-nth 9))
    (is (< (Math/pow 2 62) (fibo-nth 91) (Math/pow 2 63)))
  )

  (testing "lazy-cons"
    (let [lazy-next-int (fn lazy-next-int [n]
                          (lazy-cons n (lazy-next-int (inc n))))
          all-ints      (lazy-next-int 0)
          ]
      (is= (take 0 all-ints) [])
      (is= (take 1 all-ints) [0] )
      (is= (take 5 all-ints) [0 1 2 3 4] ))

    (let [lazy-range (fn lazy-range
                       [limit]
                       (let [lazy-range-step (fn lazy-range-step [curr limit]
                                               ; (spyx [curr limit]) (flush)
                                               (when (< curr limit)
                                                 (lazy-cons curr (lazy-range-step (inc curr) limit))))]
                         (lazy-range-step 0 limit))) ]
      (is= (lazy-range 0) nil)
      (is= (lazy-range 1) [0])
      (is= (lazy-range 5) [0 1 2 3 4]))

    (let [lazy-countdown
          (fn lazy-countdown [n]
            (when (<= 0 n)
              (lazy-cons n (lazy-countdown (dec n))))) ]
      (is= (lazy-countdown  5) [5 4 3 2 1 0] )
      (is= (lazy-countdown  1) [1 0] )
      (is= (lazy-countdown  0) [0] )
      (is= (lazy-countdown -1) nil )))

;-----------------------------------------------------------------------------
; lazy-gen/yield tests

  (let [empty-gen-fn (fn [] (lazy-gen)) ]
    (is (nil? (empty-gen-fn))))

  (let [range-gen (fn [limit] ; "A generator 'range' function."
                    (lazy-gen
                      (loop [cnt 0]
                        (when (< cnt limit)
                          (assert (= cnt (yield cnt)))
                          (recur (inc cnt))))))]

    (is= (range 1) (range-gen 1))
    (is= (range 5) (range-gen 5))
    (is= (range 10) (range-gen 10))

    ; Note different behavior for empty result
    (is= [] (range 0))
    (is= nil (range-gen 0))
    (is= (seq (range 0))
      (seq (range-gen 0))
      nil))

  (let [concat-gen        (fn [& collections]
                            (lazy-gen
                              (doseq [curr-coll collections]
                                (doseq [item curr-coll]
                                  (yield item)))))
        concat-gen-mirror (fn [& collections]
                            (lazy-gen
                              (doseq [curr-coll collections]
                                (doseq [item curr-coll]
                                  (let [items [item (- item)]]
                                    (assert (= items (yield-all items))))))))
        c1              [1 2 3]
        c2              [4 5 6]
        c3              [7 8 9]
  ]
      (is= [1 2 3 4 5 6 7 8 9] (concat-gen c1 c2 c3))
      (is= [1 -1  2 -2  3 -3  4 -4  5 -5  6 -6  7 -7  8 -8  9 -9]  (concat-gen-mirror c1 c2 c3)))

  (let [sq-yield   (fn [xs]
                     (lazy-gen
                       (doseq [x xs]
                         (yield (* x x)))))

        sq-recur   (fn [xs]
                     (loop [cum-result []
                            xs         xs]
                       (if (empty? xs)
                         cum-result
                         (let [x (first xs)]
                           (recur (conj cum-result (* x x))
                             (rest xs))))))

        sq-lazyseq (fn lazy-squares [xs]
                     (when (not-empty? xs)
                       (let [x (first xs)]
                         (lazy-seq (cons (* x x) (lazy-squares (rest xs)))))))

        sq-reduce  (fn [xs]
                     (reduce (fn [cum-result x]
                               (conj cum-result (* x x)))
                       [] xs))

        sq-for     (fn [xs]
                     (for [x xs]
                       (* x x)))

        sq-map     (fn [xs]
                     (map #(* % %) xs))

        xs         (range 5)
        res-yield  (sq-yield xs)
        res-recur  (sq-recur xs)
        res-lzsq   (sq-lazyseq xs)
        res-reduce (sq-reduce xs)
        res-for    (sq-for xs)
        res-map    (sq-map xs)
        ]
    (is= [0 1 4 9 16]
      res-yield
      res-recur
      res-lzsq
      res-reduce
      res-for
      res-map
    )
  )

  (let [heads-pairs (fn [flips]
                      (reduce +
                        (let [flips (vec flips)]
                          (lazy-gen
                            (doseq [i (range (dec (count flips)))]
                              (when (= :h (flips i) (flips (inc i)))
                                (yield 1))))))) ]
    (is= 3 (heads-pairs [:h :t :h :h :h :t :h :h])))

  ; from S. Sierra blogpost: https://stuartsierra.com/2015/04/26/clojure-donts-concat
  (let [next-results   (fn [n] (thru 1 n)) ; (thru 1 3) => [1 2 3]
        build-1        (fn [n]
                         (lazy-gen
                           (loop [counter 1]
                             (when (<= counter n)
                               (doseq [item (next-results counter)] ; #todo -> yield-all
                                 (yield item))
                               (recur (inc counter))))))
        build-2        (fn [n]
                         (lazy-gen
                           (doseq [counter (thru n)]
                             (when (<= counter n)
                               (doseq [item (next-results counter)] ; #todo -> yield-all
                                 (yield item))))))
        build-3        (fn [n]
                         (lazy-gen
                           (doseq [counter (thru n)]
                             (yield-all (next-results counter)))))
        build-result-1 (build-1 3) ; => (1 1 2 1 2 3)
        build-result-2 (build-2 3)
        build-result-3 (build-3 3) ]
    (is= [1 1 2 1 2 3] build-result-1 build-result-2 build-result-3 ))

  (let [N                 99
        cat-glue-fn      (fn [coll] (lazy-gen
                                      (yield-all (glue coll [1 2 3]))))
        iter-glue        (iterate cat-glue-fn [1 2 3]) ; #todo some sort of bug here!
        iter-glue-result (nth iter-glue N) ; #todo hangs w. 2 'yield-all' if (50 < x < 60)

        cat-flat-fn      (fn [coll] (lazy-gen
                                      (yield-all (unnest [coll [1 [2 [3]]]]))))
        iter-flat        (iterate cat-flat-fn [1 2 3]) ; #todo some sort of bug here!
        iter-flat-result (nth iter-flat N) ; #todo hangs w. 2 'yield-all' if (50 < x < 60)
        ]
    (when false
      (spyx (count iter-glue-result))
      (spyx (count iter-flat-result)))
      ; for N = 1299
      ; (count iter-glue-result) => 3900 "Elapsed time: 2453.917953 msecs"
      ; (count iter-flat-result) => 3900 "Elapsed time: 2970.726412 msecs"
    (is= iter-glue-result iter-flat-result))

; Bare yield won't compile => java.lang.RuntimeException: Unable to resolve symbol: lazy-gen-output-buffer
  ; (yield 99)

  ; (lazy-seq nil) => ()
  ; (lazy-cons 3 (lazy-seq nil)) => (3)
  ; (lazy-cons 2 (lazy-cons 3 (lazy-seq nil))) => (2 3)
  ; (lazy-cons 1 (lazy-cons 2 (lazy-cons 3 (lazy-seq nil)))) => (1 2 3)
  ;
  ; (range-gen 5) => (0 1 2 3 4)
  ; (range-gen 10) => (0 1 2 3 4 5 6 7 8 9)
  ; (concat-gen [1 2 3] [4 5 6] [7 8 9]) => (1 2 3 4 5 6 7 8 9)
  ; (empty-gen-fn) => nil

  (let [seq-of-seqs [(range  0  5)
                     (range 10 15)
                     (range 20 25)]
        flat-seq    (lazy-gen
                      (doseq [curr-seq seq-of-seqs]
                        (yield-all curr-seq)))]
    (is= flat-seq [0 1 2 3 4 10 11 12 13 14 20 21 22 23 24]))

)

;---------------------------------------------------------------------------------------------------
(dotest
  (is= [ [] [\a   \b   \c   \d   \e   \f]      ] (split-match "abcdef" "a"   ))
  (is= [ [] [\a   \b   \c   \d   \e   \f]      ] (split-match "abcdef" "ab"  ))
  (is= [    [\a] [\b   \c   \d   \e   \f]      ] (split-match "abcdef" "bc"  ))
  (is= [    [\a   \b] [\c   \d   \e   \f]      ] (split-match "abcdef" "cde" ))
  (is= [    [\a   \b   \c] [\d   \e   \f]      ] (split-match "abcdef" "de"  ))
  (is= [    [\a   \b   \c   \d] [\e   \f]      ] (split-match "abcdef" "ef"  ))
  (is= [    [\a   \b   \c   \d   \e] [\f]      ] (split-match "abcdef" "f"   ))
  (is= [    [\a   \b   \c   \d   \e   \f]  []  ] (split-match "abcdef" "fg"  ))
  (is= [    [\a   \b   \c   \d   \e   \f]  []  ] (split-match "abcdef" "gh"  ))

  (is= [    [0   1   2   3   4   5]  []  ]       (split-match (range 6) [-1]    ))
  (is= [ [] [0   1   2   3   4   5]      ]       (split-match (range 6) [0]     ))
  (is= [ [] [0   1   2   3   4   5]      ]       (split-match (range 6) [0 1]   ))
  (is= [    [0   1] [2   3   4   5]      ]       (split-match (range 6) [2 3]   ))
  (is= [    [0   1   2] [3   4   5]      ]       (split-match (range 6) [3 4 5] ))
  (is= [    [0   1   2   3] [4   5]      ]       (split-match (range 6) [4 5]   ))
  (is= [    [0   1   2   3   4] [5]      ]       (split-match (range 6) [5]     ))
  (is= [    [0   1   2   3   4   5]  []  ]       (split-match (range 6) [5 6]   ))
  (is= [    [0   1   2   3   4   5]  []  ]       (split-match (range 6) [6 7]   )))

(dotest
  (is= nil (index-using #(= [666]       %) (range 5)))
  (is= 0   (index-using #(= [0 1 2 3 4] %) (range 5)))
  (is= 1   (index-using #(= [  1 2 3 4] %) (range 5)))
  (is= 2   (index-using #(= [    2 3 4] %) (range 5)))
  (is= 3   (index-using #(= [      3 4] %) (range 5)))
  (is= 4   (index-using #(= [        4] %) (range 5)))
  (is= nil (index-using #(= [         ] %) (range 5))))

(dotest
  (is= [ [] [0   1   2   3   4]    ] (split-using #(= 0 (first %)) (range 5)))
  (is= [    [0] [1   2   3   4]    ] (split-using #(= 1 (first %)) (range 5)))
  (is= [    [0   1] [2   3   4]    ] (split-using #(= 2 (first %)) (range 5)))
  (is= [    [0   1   2] [3   4]    ] (split-using #(= 3 (first %)) (range 5)))
  (is= [    [0   1   2   3] [4]    ] (split-using #(= 4 (first %)) (range 5)))
  (is= [    [0   1   2   3   4] [] ] (split-using #(= 5 (first %)) (range 5)))
  (is= [    [0   1   2   3   4] [] ] (split-using #(= 9 (first %)) (range 5)))

  (is= [[\a \b \c] [\d \e \f]] (split-using #(starts-with? % "de") "abcdef")))

(dotest
  (let [start-segment? (fn [vals] (zero? (rem (first vals) 3))) ]
    (is= (partition-using start-segment? [1 2 3 6 7 8])
      [[1 2] [3] [6 7 8]])
    (is= (partition-using start-segment? [3 6 7 9])
      [[3] [6 7] [9]])
    (is= (partition-using start-segment? [1 2 3 6 7 8 9 12 13 15 16 17 18 18 18 3 4 5])
      [[1 2] [3] [6 7 8] [9] [12 13] [15 16 17] [18] [18] [18] [3 4 5]]))
  (throws? (partition-using even? 5)))

(dotest
  (let [ctx     (let [a 1
                      b 2
                      c 3
                      d 4
                      e 5]
                  (vals->map a b c d e)) ]
    (is= ctx {:a 1 :b 2 :c 3 :d 4 :e 5})

    (let [{:keys [a b c d e]} ctx]
      (is= [a b c d e] [1 2 3 4 5]))

    (with-map-vals ctx [a b c d e]
      (is= [a b c d e] [1 2 3 4 5])
      (is= 15 (+ a b c d e)))
    (with-map-vals ctx [b a d c e] ; order doesn't matter
      (is= [a b c d e] [1 2 3 4 5])
      (is= 15 (+ a b c d e)))

    (throws?
      (with-map-vals ctx [x y z]
        (println "shouldn't ever get here")))))


(dotest
  (is   (macro? 'and))
  (is   (macro? '->))
  (isnt (macro? '+))
  (isnt (macro? 'if)))

(dotest
  (is (submap? {:a 1} {:a 1 :b 2}))
  (is (submap? {:b 2} {:a 1 :b 2})) )

(dotest
  (let [map-ab  {:a 1 :b 2}
        map-abc {:a 1 :b 2 :c 3}]
    (is= map-ab (validate-map-keys map-ab [:a :b]))
    (is= map-ab (validate-map-keys map-ab [:a :b :x]))
    (is= map-ab (validate-map-keys map-ab #{:a :b}))
    (is= map-ab (validate-map-keys map-ab #{:a :b :x}))
    (is= map-abc (validate-map-keys map-abc [:a :b :c :x]))
    (throws? IllegalArgumentException (validate-map-keys map-ab [:a]))
    (throws? IllegalArgumentException (validate-map-keys map-ab [:b]))
    (throws? IllegalArgumentException (validate-map-keys map-ab [:a :x]))
    (throws? IllegalArgumentException (validate-map-keys map-abc [:a :b]))
    (throws? IllegalArgumentException (validate-map-keys map-abc [:a :c :x]))))

(dotest
  (let [map-123 {1 :a 2 :b 3 :c}
        tx-fn   {1 101 2 202 3 303}]
    (is= (map-keys map-123 inc) {2 :a 3 :b 4 :c})
    (is= (map-keys map-123 tx-fn) {101 :a 202 :b 303 :c}))
  (let [map-123 {:a 1 :b 2 :c 3}
        tx-fn   {1 101 2 202 3 303}]
    (is= (map-vals map-123 inc) {:a 2, :b 3, :c 4})
    (is= (map-vals map-123 tx-fn) {:a 101, :b 202, :c 303})))

(dotest
  (is= (range 5) (unlazy (range 5)))
  (let [c1 {:a 1 :b (range 3) :c {:x (range 4) (range 5) "end"}}]
    (is= c1 (unlazy c1)))
  (let [l2 '({:a ("zero" 0)} {:a ("one" 1)} {:a ("two" 2)})
        e2 (unlazy l2)]
    (is= l2 e2)
    (is= "one" (get-in e2 [1 :a 0] l2))
    ; (throws? (spyx (get-in l2 [1 :a 0] l2)))    ; #todo: SHOULD throw
    )
  (is= [1 2 3] (unlazy (map inc (range 3))))
  (is= #{1 2 3} (unlazy #{3 2 1})))

(dotest
  (let [info  {:a 1
               :b {:c 3
                   :d 4}}
        mania {:x 6
               :y {:w 333
                   :z 666}}]

    ;(spy :info-orig info)
    ;(spy :mania-orig mania)
    (it-> (destruct [info {:a ?
                           :b {:c ?
                               :d ?}}
                     mania {:y {:z ?}}] ; can ignore unwanted keys like :x
            ;(spyx [a c])
            (let [a (+ 100 a)
                  c (+ 100 c)
                  d z
                  z 777]
              ;(spyx [a c])
              (restruct-all)))
      (with-map-vals it [info mania]
        (is= info {:a 101, :b {:c 103, :d 666}})
        (is= mania {:x 6, :y {:w 333, :z 777}})))

    (it-> (destruct [info {:a ?
                           :b {:c ?
                               :d ?}}
                     mania {:y {:z ?}}] ; can ignore unwanted keys like :x
            ;(spyx [a c])
            (let [a (+ 100 a)
                  c (+ 100 c)
                  d z
                  z 777]
              ;(spyx [a c])
              (restruct info)))
      (is= it {:a 101, :b {:c 103, :d 666}}))

    (it-> (destruct [info {:a ?
                           :b {:c ?
                               :d ?}}]
            ;(spyx [a c])
            (let [a (+ 100 a)
                  c (+ 100 c)]
              ;(spyx [a c])
              (restruct)))
      (is= it {:a 101, :b {:c 103, :d 4}}))))

(dotest
  (let [info  {:a 777
               :b [2 3 4]}
        mania [{:a 11} {:b 22} {:c [7 8 9]}]]
    ;(spy :info-orig info)
    ;(spy :mania-orig mania)
    (let [z ::dummy]
      (it-> (destruct [info {:a z
                             :b [d e f]}
                       mania [{:a ?} BBB {:c clutter}]]
              ;(spyx z)
              ;(spyx [d e f])
              ;(spyx a)
              ;(spyx BBB)
              ;(spyx clutter)
              (let [clutter (mapv inc clutter)
                    BBB     {:BBB 33}
                    z       77
                    d       (+ 7 d)]
                (restruct-all)))
        (with-map-vals it [info mania]
          (is= info {:a 77, :b [9 3 4]})
          (is= mania [{:a 11} {:BBB 33} {:c [8 9 10]}]))))))

(dotest
  (let [data {:a 1
              :b {:c 3
                  :d 4}}] ; can ignore unwanted keys like :d
    (destruct [data {:a ?
                     :b {:c ?}}]
      (is= [1 3] [a c]))
    (throws?
      (destruct [data {:a ?
                       :b {:z ?}}] ; bad data example (non-existant key)
        (println [a z]))))

  (let [data [1 2 3 4 5]]
    (destruct [data [a b c]] ; can ignore unwanted indexes 3 or 4 (0-based)
      (is= [1 2 3] [a b c]))
    (destruct [data {0 a
                     2 c}] ; can destructure vectors using map-index technique
      (is= [1 3] [a c])))
  (throws?
    (let [data [1 2 3]]
      (destruct [data [a b c d]] ; bad data example (non-existant element)
        (println [a b c d]))))

  ; multi-destruct
  (let [data-1 {:a 1 :b {:c 3}}
        data-2 {:x 7 :y {:z 9}}]
    (destruct [data-1 {:a ? :b {:c ?}}
               data-2 {:x ? :y {:z ?}}]
      (is= [1 3 7 9] [a c x z])))
  (let [data-1 {:a 1 :b {:c 3}}
        data-2 [:x :y :z :666]]
    (destruct [data-1 {:a ? :b {:c ?}}
               data-2 [x y z]]
      (is= [1 3 :x :y :z] [a c x y z]))
    (destruct [[data-1 data-2]
               [item-1 item-2]]
      (is= [item-1 item-2] [data-1 data-2])))
  (let [data-1 {:a 1 :b {:c [:x :y :z :666]}}]
    (destruct [data-1 {:a ? :b {:c [x y z]}}]
      (is= [1 :x :y :z] [a x y z]))
    (destruct [data-1 {:a ? :b ?}]
      (is= a 1 )
      (is= b {:c [:x :y :z :666]}) ) )

  (let [data [{:a 1 :b {:c 3}}
              {:x 7 :y {:z 9}}]]
    (destruct [data
               [{:a ? :b {:c ?}}
                {:x ? :y {:z ?}}]]
      (is= [1 3 7 9] [a c x z])))
  (let [data {:a [{:b 2}
                  {:c 3}
                  [7 8 9]]} ]
    (destruct [data {:a [{:b p}
                         {:c q}
                         [r s t]]} ]
      (is= [2 3 7 8 9] [p q r s t])))

  ; duplicate vars
  (let [data-1 {:a 1 :b {:c 3}}
        data-2 {:x 7 :y {:c 9}}]
    (destruct [data-1 {:a ? :b {:c p}}
               data-2 {:x ? :y {:c q}}]
      (is= [1 7 3 9] [a x p q]))
    (destruct [data-1 {:a ? :b {:c ?}}
               data-2 {:x ? :y {:c q}}]
      (is= [1 7 3 9] [a x c q]))

    ; duplicate variables: these generate compile-time errors
    (comment
      (destruct [data-1 {:a ? :b {:c ?}}
                 data-2 {:x ? :y {:c ?}}]
        (println "destruct/dummy"))
      (destruct [{:a {:b {:c ?}}
                  :x {:y {:c ?}}}]
        (println "destruct/dummy")))))



; #todo move to tst.tupelo.core.deprecated
;---------------------------------------------------------------------------------------------------
; Deprecated functions

; As of Clojure 1.9.0-alpha5, seqable? is native to clojure
(dotest
  ; ^{:deprecated "1.9.0-alpha5" }
  (i/when-not-clojure-1-9-plus
    (is   (seqable?   "abc"))
    (is   (seqable?   {1 2 3 4} ))
    (is   (seqable?  #{1 2 3} ))
    (is   (seqable?  '(1 2 3) ))
    (is   (seqable?   [1 2 3] ))
    (is   (seqable?   (byte-array [1 2] )))
    (isnt (seqable?  1 ))
    (isnt (seqable? \a ))))

(dotest
  ; ^:deprecated ^:no-doc
  (let [s1    "  hello there
                 again
                 and again!   "
        r1     ["hello there"
                "again"
                "and again!"]
  ]
    (is= r1 (map str/trim (t/str->lines s1)))
    (is= r1 (map str/trim (str/split-lines s1)))))

))
