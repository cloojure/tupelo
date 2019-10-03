(ns tupelo.profile
  (:use tupelo.core))

(def ^:private timer-stats (atom {}))
(defn timer-stats-reset
  "Reset timer-stats to empty"
  [] (reset! timer-stats {}))

(defn stats-update
  "Updates timing stats for a given key & elapsed time"
  [id seconds]
  (swap! timer-stats
    (fn update-stats-fn
      [stats-map]
      (let [stats-curr (if (contains? stats-map id)
                         (grab id stats-map)
                         {:n    0
                          :sum  0.0
                          :sum2 0.0})]
        (with-map-vals stats-curr [n sum sum2]
          (let [stats-new     {:n    (inc n)
                               :sum  (+ sum seconds)
                               :sum2 (+ sum2 (* seconds seconds))}
                stats-map-new (assoc stats-map id stats-new)]
            stats-map-new))))))

(defmacro with-timer-accum
  "Accumulates execution time stats in global stats map under key `id`."
  [id & forms]
  (do
    (when-not (keyword? id)
      (throw (ex-info "id must be a keyword" (vals->map id))))
    `(let [start#   (System/nanoTime)
           result#  (do ~@forms)
           stop#    (System/nanoTime)
           elapsed# (double (- stop# start#))
           secs#    (/ elapsed# 1e9)]
       (stats-update ~id secs#)
       )))

(defmacro defnp
  "A replacement for clojure.core/defn that accumuldates profiling data. Converts a function like:

    (defnp add-1
      \"Adds one to an arg\"
      [x]
      (inc x))

  to:

    (defn add-1
      \"\"   ; dummy docstring
      [x]
      (with-timer-accum :demo.core/add-1    ; lookup key is fully-qualified function name
        (inc x)))  ; function body unchanged

  Does not handle function metadata like (defn ^:private my-fn ...) "
  [name & forms]
  (let [ns-fn-id   (keyword (str (ns-name *ns*) "/" name))
        form-0     (first forms)
        [docstring args-code] (if (string? form-0)
                                [form-0 (rest forms)]
                                ["" forms])
        args-vec   (first args-code)
        code-forms (rest args-code)]
    (list 'defn name docstring args-vec
      `(with-timer-accum ~ns-fn-id ~@code-forms))))

(defn stats-get
  "Return basic stats for a given id"
  [id]
  (let [stats-raw (grab id @timer-stats)
        n         (grab :n stats-raw)
        sum       (grab :sum stats-raw)
        sum2      (grab :sum2 stats-raw)
        mean-x    (/ sum n)
        mean2-x   (* mean-x mean-x)
        mean-x2   (/ sum2 n)
        sigma2-x  (max 0.0 (- mean-x2 mean2-x)) ; in case roundoff => neg
        sigma-x   (Math/sqrt sigma2-x)]
    {:n n :mean mean-x :sigma sigma-x}))

(defn stats-get-all
  "Return all stats"
  []
  (let [result (apply glue
                 (forv [k (keys @timer-stats)]
                   {k (stats-get k)}))]
    result))

(defn stats-print-all
  "Prints stats for all keys to stdout"
  []
  (let [stats-all-sorted (sort-by (fn sort-by-fn
                                    [mapentry]
                                    (let [[stats-key stats] mapentry]
                                      (:mean stats)))
                           (stats-get-all))]
    (doseq [[stats-key stats] stats-all-sorted]
      (let [n     (grab :n stats)
            mean  (grab :mean stats)
            sigma (grab :sigma stats)]
        (println (format "%-30s %5d %9.5f %10.8f" stats-key n mean sigma))))))

;(newline)
;(println "********************")
;(pretty (macroexpand-1
;           '(defnp add-1 [x] (inc x))))
;(println "********************")
