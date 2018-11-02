(ns todomvc.topics
  (:require
    [todomvc.enflame :as flame] ))

; NOTE:  it seems this must be in a *.cljs file or it doesn't work on figwheel reloading
(enable-console-print!)

; #todo these should all return a map

(defn register-topics! []
  (flame/define-topic! :display-mode
    [:app-state]
    (fn [app-state -query-]
      (:display-mode app-state)))

  (flame/define-topic! :sorted-todos
    [:app-state]
    (fn [app-state -query-]
      (:todos app-state)))

  (flame/define-topic! :todos
    [:sorted-todos]
    (fn [sorted-todos -query-]
      (vals sorted-todos)))

  (flame/define-topic! :visible-todos
    [:todos :display-mode]
    (fn [[todos showing] -query-]
      (let [filter-fn (condp = showing
                        :active (complement :completed)
                        :completed :completed
                        :all identity)]
        (filter filter-fn todos))))

  (flame/define-topic! :all-complete?
    [:todos]
    (fn [todos -query-]
      (every? :completed todos)))

  (flame/define-topic! :completed-count
    [:todos]
    (fn [todos -query-]
      (count (filter :completed todos))))

  (flame/define-topic! :footer-counts
    [:todos :completed-count]
    (fn [[todos completed] -query-]
      [(- (count todos) completed) completed]))

  (flame/define-topic! :ajax-response
    [:app-state]
    (fn [app-state -query-]
      (:ajax-response app-state)))

)
