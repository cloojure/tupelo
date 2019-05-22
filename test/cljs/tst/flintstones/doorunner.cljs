;*************************************************************
;*************************************************************
;*****     REMEMBER TO ENTER EACH NAMESPACE TWICE!!!     *****
;*************************************************************
;*************************************************************

(ns tst.flintstones.doorunner
  (:require
    [doo.runner :refer-macros [doo-tests]]
    [schema.core :as s]

    [tst._bootstrap]

    [tst.flintstones.dino]
    [tst.flintstones.wilma]
    [tst.flintstones.pebbles]
    [tst.flintstones.slate]
    [tst.flintstones.bambam]

    [tst.tupelo.array]
;   [tst.tupelo.chars]  ; #todo need tests
    [tst.tupelo.core]
;   [tst.tupelo.data]  ; #todo fix for cljs
    [tst.tupelo.gotchas]
    [tst.tupelo.lazy]
;   [tst.tupelo.lexical]  ; #todo fix for cljs
    [tst.tupelo.misc]
    [tst.tupelo.parse]
    [tst.tupelo.schema]
    [tst.tupelo.set]
    [tst.tupelo.string]

    [tst.tupelo.cljs.misc]
  ))

; (s/set-fn-validation! true) ; enforce fn schemas

(enable-console-print!)
(println "doorunner - beginning")

(doo-tests
  'tst._bootstrap

  'tst.flintstones.dino
  'tst.flintstones.wilma
  'tst.flintstones.pebbles
  'tst.flintstones.slate
  'tst.flintstones.bambam

  'tst.tupelo.array
; 'tst.tupelo.chars  ; #todo need tests
  'tst.tupelo.core
; 'tst.tupelo.data  ; #todo fix for cljs
  'tst.tupelo.gotchas
  'tst.tupelo.lazy
; 'tst.tupelo.lexical  ; #todo fix for cljs
  'tst.tupelo.misc
  'tst.tupelo.parse
  'tst.tupelo.schema
  'tst.tupelo.set
  'tst.tupelo.string

  'tst.tupelo.cljs.misc
)
(println "doorunner - end")
