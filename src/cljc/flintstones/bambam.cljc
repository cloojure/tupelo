(ns flintstones.bambam)

(defmacro logr-bambam
  [& body]
  `(do
     (println "logr-bambam-enter")
     (let [result# (do ~@body)]
       (println "logr-bambam-leave" result#)
       result#)))

(defn add2 [x y] (+ x y))

