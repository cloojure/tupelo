(ns todomvc.components
  "These functions are all Reagent components"
  (:require
    [goog.string :as gstring]
    [clojure.string :as str]
    [oops.core :as oops]
    [reagent.core :as r]
    [todomvc.enflame :as flame]))

; NOTE:  it seems this must be in a *.cljs file or it doesn't work on figwheel reloading
(enable-console-print!)

(def nbsp (gstring/unescapeEntities "&nbsp;")) ; get a char we can use in hiccup   ; #todo => tupelo

(defn input-field
  [{:keys [title on-save on-stop]}] ; #todo -> (with-map-vals [title on-save on-stop] ...)
  (let [text-val (r/atom title) ; local state
        stop-fn  (fn []
                   (reset! text-val "")
                   (when on-stop (on-stop)))
        save-fn  (fn []
                   (on-save (-> @text-val str str/trim))
                   (stop-fn))]
    (fn [props]
      [:input
       (merge (dissoc props :on-save :on-stop :title)
         {:type        "text"
          :value       @text-val
          :auto-focus  true
          :on-blur     save-fn
          :on-change   #(reset! text-val (flame/event-val %))
          :on-key-down #(let [rcvd (.-which %)] ; KeyboardEvent property
                          (condp = rcvd
                            flame/ascii-code-return (save-fn)
                            flame/ascii-code-escape (stop-fn)
                            nil))})])))

(defn task-list-row []
  (let [editing (r/atom false)]
    (fn [{:keys [id completed title]}]
      [:li {:class (cond-> ""
                     completed (str " completed")
                     @editing (str " editing"))}
       [:div.view
        [:input.toggle
         {:type      :checkbox
          :checked   completed
          :on-change #(flame/dispatch-event [:toggle-completed id])}]
        [:label
         {:on-double-click #(reset! editing true)}
         title]
        [:button.destroy
         {:on-click #(flame/dispatch-event [:delete-todo id])}]]
       (when @editing
         [input-field
          {:class   "edit"
           :title   title
           :on-save #(if (seq %)
                       (flame/dispatch-event [:update-title id %])
                       (flame/dispatch-event [:delete-todo id]))
           :on-stop #(reset! editing false)}])])))

(defn task-list []
  (let [visible-todos (flame/from-topic [:visible-todos :a :b])
        all-complete? (flame/from-topic [:all-complete?])]
    [:section#main
     [:input#toggle-all
      {:type      "checkbox"
       :checked   all-complete?
       :on-change #(flame/dispatch-event [:complete-all-toggle])}]
     [:label        ; #todo this does not seem to work (as a tooltip?)
      {:for "toggle-all"}
      "Mark all as complete"]
     [:ul#todo-list
      (for [todo-curr visible-todos]
        ^{:key (:id todo-curr)} [task-list-row todo-curr])]])) ; delegate to task-list-row component

; These buttons are actually hrefs (hyperliks) that will cause broswser navigation observed by History
; and propogated via secretary.
(defn footer-controls []
  (let [[num-active num-completed] (flame/from-topic [:footer-counts 1 2])
        display-mode        (flame/from-topic [:display-mode])

        ; #todo #bug doesn't visually switch from initial :all when click :active
        anchor-generator-fn (fn [filter-kw txt]
                              [:a {:class (when (= filter-kw display-mode) "selected")
                                   :href  (str "/#/" (name filter-kw))} txt])]
    [:footer#footer
     [:span#todo-count
      [:strong num-active] " " (case num-active 1 "item" "items") " left"]
     [:ul#filters
      [:li (anchor-generator-fn :all "All")]
      [:li (anchor-generator-fn :active "Active")]
      [:li (anchor-generator-fn :completed "Completed")]]
     (when (pos? num-completed)
       [:button#clear-completed
        {:on-click #(flame/dispatch-event [:clear-completed])}
        "Clear Completed"])]))

(defn task-entry []
  [:header#header
   [:h1 "todos"]
   [input-field
    {:id          "new-todo"
     :placeholder "What needs to be done?"
     :on-save     #(when-not (empty? (str/trim %))
                     (flame/dispatch-event [:add-todo %]))}]])

(defn todo-root []
  [:div
   [:section#todoapp
    [task-entry]
    (when-not (empty? (flame/from-topic [:todos]))
      [task-list])
    [footer-controls]]
   [:footer#info
    [:p "Double-click to edit a todo"]]])

;---------------------------------------------------------------------------------------------------

(defn ajax-says []
  [:div
   [:span {:style {:color :darkgreen}} [:strong "AJAX says: "]]
   [:span {:style {:font-style :italic}} nbsp nbsp (flame/from-topic [:ajax-response])]])

(defn root []       ; was simple-component
  [:div
   [:hr]
   [:div
    [:p "I am a component!"]
    [:p.someclass
     "I have " [:strong "bold"]
     [:span {:style {:color "red"}} " and red"] " text."]]
   [:hr]
   [ajax-says]
   [:hr]
   [:div
    [todo-root]]
   [:hr] ])

