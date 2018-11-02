(ns flintstones.test-cljs  ; *.cljs file makes macros available to normal CLJS code
  (:require-macros [flintstones.test-cljs]))

;*****************************************************************************
;
; IMPORTANT:  Need an empty `flintstones/test.cljs` file with (:require-macros ...) as a 
; "hook" so the compiler can find the `flintstones/test.clj[c]` file containing the macros.  
; This also allows user code to ignore difference between fn/macro in (:require ...) expression.
;
;*****************************************************************************
