;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.xxx
  "Miscellaneous functions."
  (:require 
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t]
  ))
(t/refer-tupelo)

; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs


