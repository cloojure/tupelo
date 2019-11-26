(ns tupelo.profile
  (:use tupelo.core))

(def      ; ^:private   ; #todo #awt finish
  timer-stats (atom {}))

(defn timer-stats-reset
  "Reset all timer statistics to empty"
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

(defmacro with-timer-result
  "Times execution of Clojure forms, returning a result map like:
      {:result result  :seconds seconds} "
  [& forms]
  `(let [start#   (System/nanoTime)
         result#  (do ~@forms)
         stop#    (System/nanoTime)
         elapsed# (double (- stop# start#))
         seconds# (/ elapsed# 1e9)]
     {:result  result#
      :seconds seconds#}))

(defmacro with-timer-print
  "Times execution of Clojure forms, printing the result to the screen. "
  [id & forms]
  (when-not (keyword? id)
    (throw (ex-info "id must be a keyword" (vals->map id))))
  `(let [result-map# (with-timer-result ~@forms)]
     (println (format ":with-timer-print %s %12.6f" ~id (grab :seconds result-map#)))
     (grab :result result-map#)))

(defmacro with-timer-accum
  "Times execution of Clojure forms, accumulating results in timer-stats map under key `id`."
  [id & forms]
  (when-not (keyword? id)
    (throw (ex-info "id must be a keyword" (vals->map id))))
  `(let [result-map# (with-timer-result ~@forms)]
     (stats-update ~id (grab :seconds result-map#))
     (grab :result result-map#)))

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
    {:n n :total sum :mean mean-x :sigma sigma-x}))

(defn profile-map
  "Returns a map of all profile stats, keyed by ID."
  []
  (let [stats-map     @timer-stats
        result        (apply glue (sorted-map)
                        (forv [stats-id (keys stats-map)]
                          {stats-id (stats-get stats-id)}))]
    result))

(defn profile-data-sorted
  "Returns a vector of profile stats sorted by ID."
  []
  (let [stats-sorted (vec (sort-by :id
                            (forv [[stats-id stats-data] (profile-map)]
                              (glue {:id stats-id} stats-data))))]
    stats-sorted))

(defn print-profile-stats
  "Prints profile stats to stdout."
  []
  (newline)
  (println "---------------------------------------------------------------------------------------------------")
  (println "Profile Stats:")
  (println "   Samples       TOTAL        MEAN      SIGMA           ID")
  (doseq [stats (profile-data-sorted)]
    (with-map-vals stats [id n total mean sigma]
      (println (format "%9d %12.3f %12.6f %10.6f   %-80s " n total mean sigma id))))
  (println "---------------------------------------------------------------------------------------------------")
  )




