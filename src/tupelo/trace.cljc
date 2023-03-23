(ns tupelo.trace
  (:require [tupelo.core :as t]))

; #todo make defn-trace that wraps a fn in enter/leave tracing, passing along a stack of calls
;         :enabled true/false
;         :when (fn [stack args retval] ...)
;    then log entire stack & trace data to tupelo.data


