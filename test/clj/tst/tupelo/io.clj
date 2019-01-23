;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.io
  (:use tupelo.io tupelo.core tupelo.test)
  (:require
    [tupelo.io :as tio]
    [tupelo.misc :as misc]
    [clojure.java.io :as io])
  (:import [java.nio ByteBuffer]
           [java.io ByteArrayInputStream File FileOutputStream DataOutputStream]))

(def int32  (long (+ 1e9 123456789)))
(def uint32 (long (+ 3e9 123456789)))

(def dummy-file-name "tst.tupelo.io")
(def dummy-file (tio/create-temp-file dummy-file-name))

(dotest-focus       ; #todo REMOVE focus
  (with-open [dos (DataOutputStream.
                    (FileOutputStream. dummy-file))]
    (doto dos
      (.writeInt int32)
      (.writeByte 42)
      (tio/write-string-bytes "hello")))
  (with-open [input-stream (io/input-stream dummy-file)]
    (is= int32 (take-int32 input-stream))
    (is= 42 (take-int8 input-stream))
    (is= "hello" (take-str 5 input-stream))

    )

  )
