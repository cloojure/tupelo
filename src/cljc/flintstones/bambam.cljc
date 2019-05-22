(ns flintstones.bambam)

(defmacro logr-bambam
  [& body]
  `(do
     (newline)
     (println "---------------------------------")
     (println "logr-bambam-enter")
     (let [result# (do ~@body)]
       (println "logr-bambam-leave" result#)
       (println "---------------------------------")
       (newline)
       result#)))

(defn add2 [x y] (+ x y))

