;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.spec
  "Miscellaneous functions."
  #?@(:clj [
  (:require
    [clojure.string :as str]
   ;[clojure.spec.alpha :as sp]
   ;[clojure.spec.test.alpha :as stest]
   ;[clojure.spec.gen.alpha :as gen]
   ;[tupelo.impl :as i]
  )
  ]) )

; #todo  possible replacements:
; #todo    =>  spec/anything
; #todo    =>  spec/pass-all (vs. spec/pass-none)
; #todo    =>  spec/dont-care
; #todo    =>  (constantly true)
; #todo    =>  #(fn [& _] true)
; #todo    =>  clojure.core/->true (vs. clojure.core/->false)
; #todo    =>  clojure.core/true-fn (vs. clojure.core/false-fn)
; #todo    =>  (s/def ::s/anything (constantly true))
; #todo    =>  (s/def ::s/nothing (constantly false))

; #todo     (s/fdef clojure.core/declare
; #todo       :args  (s/cat :names (s/* simple-symbol?))
; #todo       :ret   any?)



