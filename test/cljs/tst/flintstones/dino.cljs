(ns tst.flintstones.dino
  (:require
    [flintstones.test-cljs :refer [dotest is isnt is= isnt= testing define-fixture]]
    [dinoPhony] ))

(define-fixture :each
  {:enter (fn [ctx] (println "*** TEST EACH *** - enter ctx=" ctx))
   :leave (fn [ctx] (println "*** TEST EACH *** - leave ctx=" ctx))})

(dotest (is= 5 (+ 2 3)))
; (deftest t-will-fail (is (= 95 (+ 2 3))))

(dotest
  (println "globalObject:  " js/globalObject)
  (let [result-7 (-> js/globalObject .-b (+ 5))
        dino     (js/makeDino)
        desc     (.-desc dino)
        said     (.says dino 5)]
    (println "(-> % .-b (+ 5) =>" result-7)
    (println "(js/makeDino) =>" dino)
    (println "dino.desc => " desc)
    (println "dino.says(5) => " said)
    (is= 7 result-7)
    (is= desc "blue dino-dog")
    (is= said "Ruff-Ruff-Ruff-Ruff-Ruff!")))

(dotest
  (let [tgt-word-re #"\b\w*a\w*\b"
        words       ["I" "am" "having" "some" "fun" "today"]
        keep?       (fn [word] (re-find tgt-word-re word))
        keep-words  (filter keep? words)]
    (prn :keep-words keep-words)
    (prn :re-seq (re-seq tgt-word-re "I am having some fun today")))

  (is= 3 (+ 2 1)))

