(ns ^:eftest/synchronized clj-lp.core-test
  (:require [clojure.test :refer :all]
            [clj-lp.core :refer :all]))

(defn approx
  ([x y]
   (approx x y 0.01))
  ([x y e]
   (-> (Math/abs (- x y))
       (< e))))

(defn example-1 []
  (let [objective-function
        {:linear-terms {:x 1} :constant-term 0}
        constraints
        [{:lhs {:x 1} :rel '<= :rhs 3}
         {:lhs {:x 1} :rel '>= :rhs 0}]]
    (minimize objective-function constraints)))

(deftest example-1-test
  (is (= (example-1) {:x 0.0})))

(defn example-2 []
  "Same as java_interop_test/example-2."
  (let [objective-function {:linear-terms {:x 1 :y 1} :constant-term 0}
        constraints
        [{:lhs {:x 1 :y 1} :rel '<= :rhs 0.75}
         {:lhs {:x 1} :rel '>= :rhs 0}
         {:lhs {:y 1} :rel '>= :rhs 0}
         {:lhs {:x 1} :rel '<= :rhs 0.5}
         {:lhs {:y 1} :rel '<= :rhs 1}]]
    (maximize objective-function constraints)))

(deftest example-2-test
  (is (= (example-2) {:x 0.5, :y 0.25})))

(defn example-3 []
  "From Chvatal, Linear Programming, "
  (let [objective-function
        {:linear-terms {:x1 5 :x2 5 :x3 3} :constant-term 0}
        constraints
        [{:lhs {:x1 1 :x2 3 :x3 1} :rel '<= :rhs 3}
         {:lhs {:x1 -1 :x3 3} :rel '<= :rhs 2}
         {:lhs {:x1 2 :x2 -1 :x3 2} :rel '<= :rhs 4}
         {:lhs {:x1 2 :x2 3 :x3 -1} :rel '<= :rhs 2}
         {:lhs {:x1 1} :rel '>= :rhs 0}
         {:lhs {:x2 1} :rel '>= :rhs 0}
         {:lhs {:x3 1} :rel '>= :rhs 0}]]
    (maximize objective-function constraints)))

(deftest example-3-test
  (let [solution (example-3)]
    (is (approx (:x1 solution) 1.103))
    (is (approx (:x2 solution) 0.275))
    (is (approx (:x3 solution) 1.034))))

(defn example-4 []
  "From J E Beasley, http://people.brunel.ac.uk/~mastjjb/jeb/or/morelp.html.

   max:
   17.1667x + 25.8667y
   subject to:
   13x + 19y <= 2400
   20x + 29y <= 2100
   x >= 10
   x,y >= 0

   value = 1866.381827586207
   x = 10.0
   y = 65.51724137931035"
  (let [objective-function
        {:linear-terms {:x 17.1667 :y 25.8667} :constant-term 0}
        constraints
        [{:lhs {:x 1} :rel '>= :rhs 10}
         {:lhs {:x 1} :rel '>= :rhs 0}
         {:lhs {:y 1} :rel '>= :rhs 0}
         {:lhs {:x 13 :y 19} :rel '<= :rhs 2400}
         {:lhs {:x 20 :y 29} :rel '<= :rhs 2100}]]
    (maximize objective-function constraints)))

(deftest example-4-test
  (let [solution (example-4)]
    (is (approx (:x solution) 10.000))
    (is (approx (:y solution) 65.517))))

