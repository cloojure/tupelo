(ns flintstones.slate)

(defn add2 [x y] (+ x y))

(defmacro logr-slate
  [& body]
  `(do
     (newline)
     (println "---------------------------------")
     (println "logr-slate-enter")
     (let [result# (do ~@body)]
       (println "logr-slate-leave" result#)
       (println "---------------------------------")
       (newline)
       result#)))

