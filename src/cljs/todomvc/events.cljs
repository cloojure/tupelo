(ns todomvc.events
  (:require
    [oops.core :as oops]
    [todomvc.app-state :as app-state]
    [todomvc.enflame :as flame] ))

; NOTE:  it seems this must be in a *.cljs file or it doesn't work on figwheel reloading
(enable-console-print!)

(def common-interceptors
  [app-state/check-spec-intc
   app-state/localstore-save-intc
   flame/ajax-intc
   ;flame/trace
   flame/trace-print
  ])

; This event is dispatched when the app's `main` ns is loaded (todomvc.core). It establishes
; initial application state in the context map `:app-state` key. That means merging:
;   1. Any todos stored in the browser's LocalStore (from the last session of this app)
;   2. Default initial values
(defn initialise-app-state [ctx -event-]
  (js/console.log :initialise-app-state :enter ctx)
  (let [local-store-todos (flame/get-in-strict ctx [:local-store-todos])
        initial-state     (into app-state/default-state {:todos local-store-todos})
       ;initial-state     app-state/default-state ; ability to reset LocalStore during development
        ctx-out           (into ctx {:app-state initial-state})]
    (js/console.log :initialise-app-state :leave ctx-out)
    ctx-out))

; #todo need plumatic schema and tsk/KeyMap
(defn set-display-mode
  "Saves current 'showing' mode (3 filter buttons at the bottom of the display)"
  [ctx [-e- new-filter-kw]] ; :- #{ :all, :active or :completed }
  (assoc-in ctx [:app-state :display-mode] new-filter-kw))

(defn add-todo [ctx [-e- todo-title]]
  (update-in ctx [:app-state :todos] ; #todo make this be (with-path ctx [:app-state :todos] ...) macro
    (fn [todos]     ; #todo kill this part
      ; must choose a new id greater than any existing id (possibly from localstore todos)
      (let [todo-ids (keys todos)
            new-id   (if (not-empty todo-ids)
                       (inc (apply max todo-ids))
                       0)]
        (into todos {new-id {:id new-id :title todo-title :completed false}})))))

(defn toggle-completed [ctx [-e- todo-id]]
  (update-in ctx [:app-state :todos todo-id :completed] not))

(defn update-title [ctx [-e- todo-id todo-title]]
  (assoc-in ctx [:app-state :todos todo-id :title] todo-title))

(defn delete-todo [ctx [-e- todo-id]]
  (flame/dissoc-in ctx [:app-state :todos todo-id]))

(defn clear-completed-todos
  [ctx -event-]
  (let [todos     (get-in ctx [:app-state :todos])
        completed-ids  (->> (vals todos) ; find id's for todos where (:completed -> true)
                    (filter :completed)
                    (map :id))
        todos-new (reduce dissoc todos completed-ids) ; delete todos which are completed
        result    (assoc-in ctx [:app-state :todos] todos-new)]
    result))

(defn toggle-completed-all
  "Toggles the completed status for each todo"
  [ctx -event-]
  (let [todos     (get-in ctx [:app-state :todos])
        new-completed  (not-every? :completed (vals todos)) ; work out: toggle true or false?
        todos-new (reduce #(assoc-in %1 [%2 :completed] new-completed)
                    todos
                    (keys todos))
        result    (assoc-in ctx [:app-state :todos] todos-new)]
    result))

(defn register-handlers! []
  (flame/event-handler-for! :initialize-state   ; usage: (flame/dispatch-event [:initialise-state])
    [app-state/localstore-load-intc app-state/check-spec-intc]
    initialise-app-state)

  (flame/event-handler-for! :set-display-mode ; receives events from URL changes via History/secretary
    [app-state/check-spec-intc]
    set-display-mode)

  (flame/event-handler-for! :add-todo
    common-interceptors
    add-todo)

  (flame/event-handler-for! :toggle-completed
    common-interceptors
    toggle-completed)

  (flame/event-handler-for! :update-title
    common-interceptors
    update-title)

  (flame/event-handler-for! :delete-todo
    common-interceptors
    delete-todo)

  (flame/event-handler-for! :clear-completed
    common-interceptors
    clear-completed-todos)

  (flame/event-handler-for! :complete-all-toggle
    common-interceptors
    toggle-completed-all)

  (flame/event-handler-for! :ajax-demo
    common-interceptors
    (fn [ctx [-e- method uri opts]]
      (assoc ctx :ajax (into {:method method :uri uri} opts))))

  (flame/event-handler-for! :ajax-response
    common-interceptors
    (fn [ctx [-e- response]]
      (assoc-in ctx [:app-state :ajax-response] response)))


  )
