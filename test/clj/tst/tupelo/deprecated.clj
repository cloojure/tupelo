(ns tst.tupelo.deprecated
  (:use tupelo.deprecated tupelo.core tupelo.test )
  (:require [clojure.string :as str]))

;---------------------------------------------------------------------------------------------------
; Deprecated functions

; As of Clojure 1.9.0-alpha5, seqable? is native to clojure
(verify
  ; ^{:deprecated "1.9.0-alpha5" }
  (when-not-clojure-1-9-plus
    (is   (seqable?   "abc"))
    (is   (seqable?   {1 2 3 4} ))
    (is   (seqable?  #{1 2 3} ))
    (is   (seqable?  '(1 2 3) ))
    (is   (seqable?   [1 2 3] ))
    (is   (seqable?   (byte-array [1 2] )))
    (isnt (seqable?  1 ))
    (isnt (seqable? \a ))))

(verify
  ; ^:deprecated ^:no-doc
  (let [s1    "  hello there
                 again
                 and again!   "
        r1     ["hello there"
                "again"
                "and again!"]
        ]
    (is= r1 (map str/trim (str->lines s1)))
    (is= r1 (map str/trim (str/split-lines s1)))))

