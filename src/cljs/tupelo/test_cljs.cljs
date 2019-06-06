(ns tupelo.test-cljs
  (:require-macros [tupelo.test-cljs]))
; Having both a `*.cljc` and a `*.cljs` file for the same namespace makes macros
; available to normal CLJS code without needing the `:include-macros true`.
; See "Implicit macro loading" at https://clojurescript.org/about/differences#_namespaces

;*****************************************************************************
;
; IMPORTANT:  Need an empty `tupelo/test.cljs` file with (:require-macros ...) as a 
; "hook" so the compiler can find the `tupelo/test.clj[c]` file containing the macros.  
; This also allows user code to ignore difference between fn/macro in (:require ...) expression.
;
;*****************************************************************************
