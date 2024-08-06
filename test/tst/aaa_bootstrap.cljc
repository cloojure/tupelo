;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.aaa-bootstrap
  "This namespace is used to perform one-time tasks during testing, such as printing the
  Clojure version."
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [clojure.test.check.clojure-test :as tst]
    [schema.core :as s]
    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty]]
    [tupelo.test :refer [deftest testing dotest verify is isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? ]])
  )

#?(:cljs (enable-console-print!))

(s/set-fn-validation! true) ; enforce fn schemas

; Suppress reporting for successful runs of `test.check`.
; Before:
;     Testing tst.tupelo.y64
;     {:result true, :num-tests 999, :seed 1722977330290, :time-elapsed-ms 44, :test-var "dospec-line-48"}
;     {:result true, :num-tests 999, :seed 1722977330335, :time-elapsed-ms 43, :test-var "dospec-line-58"}
;
; After:
;     Testing tst.tupelo.y64
;
; Test errors still generate reports
(alter-var-root (var tst/*report-completion*) (constantly false))

; Prismatic Schema type definitions
; #todo add to Schema docs
; (set! *warn-on-reflection* true)  ; #todo enable?

(verify
  (t/print-versions)
  ;(spyx (s/fn-validation?))

  (is (= 5 (+ 2 3)))
  (isnt (= 2 3))
  )
