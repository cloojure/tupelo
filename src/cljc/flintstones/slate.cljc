(ns flintstones.slate)

(defn add2 [x y] (+ x y))

(defmacro logr-slate
  [& body]
  `(do
     (println "logr-slate-enter")
     (let [result# (do ~@body)]
       (println "logr-slate-leave" result#)
       result#)))

