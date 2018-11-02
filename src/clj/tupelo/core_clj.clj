(ns tupelo.core-clj
  (:import [java.io BufferedReader StringReader ByteArrayOutputStream PrintStream])
  )


;---------------------------------------------------------------------------------------------------
; DEPRECATED functions

; duplicate of str/split-lines
(defn ^:deprecated ^:no-doc str->lines
  "***** DEPRECATED:  duplicate of str/split-lines *****

  Returns a lazy seq of lines from a string"
  [string-arg]
  (line-seq (BufferedReader. (StringReader. string-arg))))


