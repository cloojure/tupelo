;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.io
  (:use tupelo.io tupelo.core tupelo.test)
  (:refer-clojure :exclude [read-string])
  (:require
    [clojure.java.io :as io] )
  (:import [java.io
              DataInputStream DataOutputStream
              File FileOutputStream FileInputStream
              InputStream OutputStream ]))

(def int-val  (long (+ 1e9 123456789)))
(def long-val  (long 12345e9))

(def dummy-file-name "tst.tupelo.io")
(def dummy-file (create-temp-file dummy-file-name))

(dotest-focus       ; #todo FOCUS
  (let [in-stream  (io/input-stream dummy-file)
        out-stream (io/output-stream dummy-file)
        dis        (DataInputStream. in-stream)
        dos        (DataOutputStream. out-stream)]
    (isnt (data-input-stream? in-stream))
    (is (input-stream? in-stream))
    (is (input-stream? dis))
    (is (data-input-stream? dis))

    (isnt (data-output-stream? out-stream))
    (is (output-stream? out-stream))
    (is (output-stream? dos))
    (is (data-output-stream? dos))))

(dotest-focus       ; #todo FOCUS
  (with-open [dos (DataOutputStream.
                    (FileOutputStream. dummy-file))]
    (doto dos
      (write-int int-val)
      (write-long long-val)
      (write-byte 42)
      (write-byte-unsigned 255)
      (write-byte -1)
      (write-string-bytes "hello")))
  (with-open [dis (DataInputStream. (io/input-stream dummy-file))]
    (is= int-val (read-int dis))
    (is= long-val (read-long dis))
    (is= 42 (read-byte dis))
    (is= 255 (read-byte-unsigned dis))
    (is= -1 (read-byte dis))
    (is= "hello" (read-string-bytes 5 dis))

    )
)
