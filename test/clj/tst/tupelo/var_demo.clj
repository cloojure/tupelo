(ns tst.tupelo.var-demo
  (:use tupelo.core tupelo.test))

(def wilma 3)
; `wilma` is a global symbol that points to
;     an Var object, that points to
;         a java.lang.Long object of value `3`

(dotest
  (is= java.lang.Long (type wilma))

  (is= 3 (var-get (var wilma)))
  (is= 3 (var-get #'wilma))

  ; `deref` and `var-get` are interchangable
  (is= 3 (deref (var wilma)))
  (is= 3 (deref #'wilma))

  ; the reader macro `@xxx` is a shortcut that translates to `(deref xxx)`
  (is= 3 @(var wilma))
  (is= 3 @#'wilma)) ; Don't do this -  it's an abuse of reader macros.

(defn add-3 [x] (+ x 3))
; `add-3` is a global symbol that points to
;     an Var object, that points to
;         an function object.

(dotest
  (let [add-3-fn  add-3 ; a local pointer to the fn object
        add-3-var (var add-3)] ; a local pointer to the Var object
    (is= 42 (add-3 39)) ; double deref from global symbol to fn object
    (is= 42 (add-3-fn 39)) ; single deref from local  symbol to fn object
    (is= 42 (add-3-var 39))) ; use the Var object as a function => SILENT deref to fn object

  (let [wilma-long wilma ; a local pointer to the long object
        wilma-var  (var wilma)] ; a local pointer to the Var object
    (is (int? wilma-long))
    (is (var? wilma-var))

    (is= 4 (inc wilma)) ; double deref from global symbol to Long object
    (is= 4 (inc wilma-long)) ; single deref from local symbol to Long object
    (throws? (inc wilma-var)))) ; Var object used as arg => WILL NOT deref to Long object

(defn unvar
  "When passed a clojure var-object, returns the referenced value (via deref/var-get);
  else returns arg unchanged. Idempotent to multiple calls."
  [value-or-var]
  (if (var? value-or-var)
    (deref value-or-var) ; or var-get
    value-or-var))

(dotest
  (let [wilma-long wilma ; a local pointer to the long object
        wilma-var  (var wilma)] ; a local pointer to the Var object
    (is= 42 (+ 39 (unvar wilma))
      (+ 39 (unvar wilma-long))
      (+ 39 (unvar wilma-var)))
    ))



