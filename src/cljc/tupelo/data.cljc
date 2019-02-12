(ns tupelo.data
  #?(:clj
     (:use tupelo.core tupelo.test))
  )

; #todo be able to process this data & delete unwise users

(def customers
  [{:customer-id 1
    :plants      [{:plant-id  1
                   :employees [{:name "Alice" :age 35 :sex "F"}
                               {:name "Bob" :age 25 :sex "M"}]}
                  {:plant-id  2
                   :employees []}]}
   {:customer-id 2}])

(def age-of-wisdom 30)
