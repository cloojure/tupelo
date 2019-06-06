(ns tupelo.macros)

(defn cljs-env?     ; from plumatic schema/macros.clj
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

(defmacro if-cljs     ; from plumatic schema/macros.clj
  "Return `then` if we are generating cljs code and `else` for Clojure code.
   https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
  [then else]
  (if (cljs-env? &env) then else))

(defmacro try-catchall     ; from plumatic schema/macros.clj
  "A cross-platform variant of try-catch that catches all exceptions.
   Does not (yet) support finally, and does not need or want an exception class."
  [& body]
  (let [try-body (butlast body)
        [catch-op ex-symbol & catch-body :as catch-form] (last body)]
    (assert (= catch-op 'catch))
    (assert (symbol? ex-symbol))
    `(if-cljs
       (try ~@try-body (catch js/Object ~ex-symbol ~@catch-body))
       (try ~@try-body (catch Throwable ~ex-symbol ~@catch-body)))))


