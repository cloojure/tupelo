;*************************************************************
;*************************************************************
;*****     REMEMBER TO ENTER EACH NAMESPACE TWICE!!!     *****
;*************************************************************
;*************************************************************

(ns tst.flintstones.doorunner
  (:require
    [doo.runner :refer-macros [doo-tests]]
    [schema.core :as s]

    [tst.flintstones.dino]
    [tst.flintstones.wilma]
    [tst.flintstones.pebbles]
    [tst.flintstones.slate]
    [tst.flintstones.bambam]

    [tst.tupelo._bootstrap]
    [tst.tupelo.core]
    [tst.tupelo.set]
    [tst.tupelo.lazy]
    [tst.tupelo.string]
   ;[tst.tupelo.xxx]
  ))

; (s/set-fn-validation! true) ; enforce fn schemas

(enable-console-print!)
(println "doorunner - beginning")

(doo-tests
  'tst.flintstones.dino
  'tst.flintstones.wilma
  'tst.flintstones.pebbles
  'tst.flintstones.slate
  'tst.flintstones.bambam

  'tst.tupelo._bootstrap
  'tst.tupelo.core
  'tst.tupelo.set
  'tst.tupelo.lazy
  'tst.tupelo.string
 ;'tst.tupelo.xxx
)
(println "doorunner - end")
