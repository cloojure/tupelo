;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.async
  #?@(:clj [
  (:use tupelo.test)
  (:require [tupelo.impl            :as i]
            [tupelo.async           :as ta]
            [clojure.core.async     :as ca :refer [go go-loop chan thread]]
  )
            ]) )

(defn is-expected-val? [tst-val arg]
  (is (= tst-val arg)))

(deftest t-basic
  (is= 42 (i/only (ta/vec (go 42)))) ; returns a channel containing its return value
  (is= 42 (i/only (ta/vec (thread 42)))) ; returns a channel containing its return value

  ; can create real threads
  (let [ch (chan)]  ; zero-buffer channel
    (thread
      (ta/put-now! ch "here") ; these will block
      (ta/put-now! ch "we")
      (ta/put-now! ch "go")
      (ca/close! ch))
    (is= "here" (ta/take-now! ch))
    (is= "we"   (ta/take-now! ch))
    (is= "go"   (ta/take-now! ch))
    (is= nil    (ta/take-now! ch)))

  ; or "lightweight" threads
  (let [ch (chan)]  ; zero-buffer channel
    (go (ta/put-go! ch "here"  )
        (ta/put-go! ch "we")
        (ta/put-go! ch "go")
        (ta/put-go! ch "again"))
    (is= ["here" "we" "go" "again"] ; vector ok since order determinate within go block
      (ta/take-now!
        (go [ (ta/take-go! ch)
              (ta/take-go! ch)
              (ta/take-go! ch)
              (ta/take-go! ch) ] ))))

  ; or "lightweight" threads
  (let [ch (chan)]  ; zero-buffer channel
    (go (doseq [val ["here" "we" "go" "again"]]
          (ta/put-now! ch val)))
    (is= ["here" "we" "go" "again"] ; vector ok since order determinate within go block
         (for [_ (range 4)]
           (ta/take-now! ch))))

  (let [ch (chan 1) ]
    (go
      (ta/put-go! ch 42)
      (is= 42 (ta/take-go! ch))))

  (let [c9 (chan 9) ]
    (ta/put-now!   c9 88)
    (ta/put-later! c9 99)

    (is= 88 (ta/take-now! c9))
    (ta/take-later! c9 (partial is-expected-val? 99)))

  (let [ c0 (chan) ]
    ; #todo clean up this example
    (ta/put-later! c0 10 (partial is-expected-val? true))
    (ta/put-later! c0 11 (partial is-expected-val? true))
    (ta/put-later! c0 12 (partial is-expected-val? true))

    (is= 10 (ta/take-now!  c0))
    (is= 11 (ta/take-now!  c0))
    (is= 12 (ta/take-now!  c0))

    (ca/close! c0)
    (is (false? (ta/put-later! c0 -1 (partial is-expected-val? false) )))
    (is (nil? (ta/take-now!  c0))))

  (let [ch-0  (ca/to-chan (range 5))
        ch-1  (chan 99)
  ]
    (is= [0 1 2 3 4] (ta/vec ch-0))
    (doseq [val (range 5)]
      (ta/put-now! ch-1 val))
    (ca/close! ch-1)
    (is= [0 1 2 3 4] (ta/vec ch-1)))

  (let [ch-in  (chan)
        ch-out (chan 99) ]
    (go-loop [cnt (ta/take-go! ch-in) ] ; same as (go (loop ...))
      (ta/put-go! ch-out cnt)
      (if (pos? cnt)
        (recur (dec cnt))
        (do (ca/close! ch-in)
            (ca/close! ch-out))))
    (ta/put-later! ch-in 3)
    (is= [3 2 1 0] (ta/vec ch-out)))

  ; put-go! & take-go!
  (let [ch-pipe  (chan)
        ch-out (chan 99) ]
    (go-loop [cnt 3]
      (ta/put-go! ch-pipe cnt)
      (if (pos? cnt)
        (recur (dec cnt))
        (ca/close! ch-pipe)))
    (go-loop [val (ta/take-go! ch-pipe)]
      (if (i/not-nil? val)
        (do (ta/put-go! ch-out val)
            (recur (ta/take-go! ch-pipe)))
        (ca/close! ch-out)))
    (is= [3 2 1 0] (ta/vec ch-out)))

  ; put-now! & take-now!
  (let [ch-pipe  (chan)
        ch-out (chan 99) ]
    (go-loop [cnt 3]
      (ta/put-now! ch-pipe cnt)
      (if (pos? cnt)
        (recur (dec cnt))
        (ca/close! ch-pipe)))
    (go-loop [val (ta/take-now! ch-pipe)]
      (if (i/not-nil? val)
        (do (ta/put-now! ch-out val)
            (recur (ta/take-now! ch-pipe)))
        (ca/close! ch-out)))
    (is= [3 2 1 0] (ta/vec ch-out)))

  ; put-later! & take-now!  (NOTE: generally do not want to use take-later! function)
  (let [ch-pipe  (chan)
        ch-out (chan 99) ]
    (go-loop [cnt 3]
      (ta/put-later! ch-pipe cnt)
      (if (pos? cnt)
        (recur (dec cnt))
        (ca/close! ch-pipe)))
    (go-loop [val (ta/take-now! ch-pipe)]
      (if (i/not-nil? val)
        (do (ta/put-later! ch-out val)
            (recur (ta/take-now! ch-pipe)))
        (ca/close! ch-out)))
    (is= [3 2 1 0] (ta/vec ch-out)))
)
; #todo maybe close! -> close-now! if current thread?

;-----------------------------------------------------------------------------
; demonstrate pipeline, split

(defn err-3 [x]
  "'fail' for multiples of 3"
  (if (zero? (mod x 3))
    (+ x 300)       ; error case
    x))             ; non-error

(defn err-5 [x]
  "'fail' for multiples of 5"
  (if (zero? (mod x 5))
    (+ x 500)       ; error case
    x))             ; non-error

(defn is-ok?
  "Returns true if the value is 'in error' (>=100)"
  [x]
  (< x 100))

(deftest t-pipeline-split

  (let [
    ch-0                      (ca/to-chan (range 10))
    ch-1                      (chan 99)
    ch-2                      (chan 99)
    _                         (ca/pipeline 1 ch-1 (map err-5) ch-0)
    [ok-chan-1 fail-chan-1]   (ca/split is-ok? ch-1 99 99)
    _                         (ca/pipeline 1 ch-2 (map err-3) ok-chan-1)
    [ok-chan-2 fail-chan-2]   (ca/split is-ok? ch-2 99 99)

    ok-vec-2                  (ta/vec ok-chan-2)
    fail-vec-1                (ta/vec fail-chan-1)
    fail-vec-2                (ta/vec fail-chan-2)
  ]
    (is (= ok-vec-2     [1 2 4 7 8]))
    (is (= fail-vec-1   [500 505]))
    (is (= fail-vec-2   [303 306 309])))
)
