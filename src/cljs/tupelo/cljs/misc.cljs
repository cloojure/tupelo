;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.cljs.misc
  "Tupelo - Making Clojure even sweeter"
  (:require
    [tupelo.core :as t]))

(defn grouper
  "Uses js/RegExp to find matching groups.  Sample output (using single-quotes):

    (grouper #'[a-z0-9][A-Z]'  'aTaTa')  =>
      [ {:groups ['aT']  :match 'aT'  :index 0  :last-index 2  :input 'aTaTa' }
        {:groups ['aT']  :match 'aT'  :index 2  :last-index 4  :input 'aTaTa' } ]

    (grouper  #'((\d+)-(\d+))' '672-345-456-3212')  =>
      [ {:groups ['672-345'  '672-345'  '672' '345' ]  :match '672-345'   :index 0  :last-index  7  :input '672-345-456-3212' }
        {:groups ['456-3212' '456-3212' '456' '3212']  :match '456-3212'  :index 8  :last-index 16  :input '672-345-456-3212' } ]

  Note that the JS value returned by `:last-index` is the index of the first char in the input string *after* the current match.
  "
  [re input-str]
  (let [re-src re.source] ; the source string from the regexp arg
    (loop [groups []
           regexp (js/RegExp. re-src "g")] ; 'g' => global search
      (let [res     (.exec regexp input-str)
            res-clj (js->clj res)]
        (if (nil? res)
          groups
          (recur
            (conj groups {:groups     res-clj :match (get res-clj 0) :index res.index :input res.input
                          :last-index regexp.lastIndex})
            regexp))))))

(dotest
  (newline)
  (let [value        "aTaTa"
        pat          #"[a-z0-9][A-Z]"
        phone-number "672-345-456-3212"
        phone-pat   #"((\d+)-(\d+))" ]
    (println \newline :aTaTa)
    (prn (grouper pat value ))
    (println \newline :phone)
    (prn (grouper phone-pat phone-number ))
    ))

(comment


(grouper #"[a-z0-9][A-Z]"  "aTaTa")  =>
    [ {:groups ["aT"]  :match "aT"  :index 0  :last-index 2  :input "aTaTa" }
      {:groups ["aT"]  :match "aT"  :index 2  :last-index 4  :input "aTaTa" } ]

(grouper  #"((\d+)-(\d+))" "672-345-456-3212")  =>
    [ {:groups ["672-345"  "672-345"  "672" "345" ]  :match "672-345"   :index 0  :last-index  7  :input "672-345-456-3212" }
      {:groups ["456-3212" "456-3212" "456" "3212"]  :match "456-3212"  :index 8  :last-index 16  :input "672-345-456-3212" } ]
  
  
