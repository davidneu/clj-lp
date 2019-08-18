(ns ^:eftest/synchronized clj-lp.java-interop-test
  (:require [clojure.test :refer :all])
  (:import [org.apache.commons.math3.optim.linear
            SimplexSolver
            LinearObjectiveFunction
            LinearConstraint
            NonNegativeConstraint
            LinearConstraintSet
            Relationship
            UnboundedSolutionException
            NoFeasibleSolutionException]
           [org.apache.commons.math3.optim.nonlinear.scalar GoalType]
           [org.apache.commons.math3.optim OptimizationData]))




(defn example-1 []
  "https://opensource.googleblog.com/2009/06/introducing-apache-commons-math.html

   min -2x + y - 5
   s.t.
   x + 2y <= 6
   3x + 2y <= 12
   y >= 0

   LinearObjectiveFunction f = new LinearObjectiveFunction(new double[] { -2, 1 }, -5);
   Collection constraints = new ArrayList();
   constraints.add(new LinearConstraint(new double[] { 1, 2 }, Relationship.LEQ, 6));
   constraints.add(new LinearConstraint(new double[] { 3, 2 }, Relationship.LEQ, 12));
   constraints.add(new LinearConstraint(new double[] { 0, 1 }, Relationship.GEQ, 0));

   // create and run the solver
   RealPointValuePair solution = new SimplexSolver().optimize(f, constraints, GoalType.MINIMIZE, false);

   // get the solution
   double x = solution.getPoint()[0];
   double y = solution.getPoint()[1];
   double min = solution.getValue();

   dev=> (def solution (clj-lp.java-interop-test/example-1))
   #object[org.apache.commons.math3.optim.PointValuePair 0x56582f60 '[[D@2d010ba, -3.0]']

   dev=> (get (.getPoint solution) 0)
   4.0

   dev=> (get (.getPoint solution) 1)
   0.0

   dev=> (.getValue solution)
   -3.0"
  (let [f (LinearObjectiveFunction. (double-array 2 [-2 1]) 5.0)
        constraints (LinearConstraintSet.
                     [(LinearConstraint. (double-array 2 [1 2]) Relationship/LEQ 6.0)
                      (LinearConstraint. (double-array 2 [3 2]) Relationship/LEQ 12.0)
                      (LinearConstraint. (double-array 2 [0 1]) Relationship/GEQ 0.0)])
        optimization-data (into-array OptimizationData [f constraints GoalType/MINIMIZE])]
    (.optimize (SimplexSolver.) optimization-data)))

(deftest example-1-test
  (let [solution (example-1)]
    (is (= (get (.getPoint solution) 0) 4.0))
    (is (= (get (.getPoint solution) 1) 0.0))
    (is (= (.getValue solution) -3.0))))

(defn example-2 []
  "max: x + y;
   x + y <= 0.75;
   x >= 0;
   x <= 0.5;
   y >= 0;
   y <= 1;
   x = 0.5, y = 0.25, value = 0.75

   dev=> (def solution (clj-lp.java-interop-test/example-2))

   dev=> (get (.getPoint solution) 0)
   0.5

   dev=> (get (.getPoint solution) 1)
   0.25

   dev=> (.getValue solution)
   0.75

   Notice degeneracy and that x=0, y=0.75 is also a solution."
  (let [f (LinearObjectiveFunction. (double-array 2 [1 1]) 0.0)
        
        constraints (LinearConstraintSet.
                     [(LinearConstraint. (double-array 2 [1 1]) Relationship/LEQ 0.75)
                      (LinearConstraint. (double-array 2 [1 0]) Relationship/GEQ 0.0)                      
                      (LinearConstraint. (double-array 2 [1 0]) Relationship/LEQ 0.5)
                      (LinearConstraint. (double-array 2 [0 1]) Relationship/GEQ 0.0)
                      (LinearConstraint. (double-array 2 [0 1]) Relationship/LEQ 1.0)])
        optimization-data (into-array OptimizationData [f constraints GoalType/MAXIMIZE])]
    (.optimize (SimplexSolver.) optimization-data)))

(deftest example-2-test
  (let [solution (example-2)]
    (is (= (get (.getPoint solution) 0) 0.5))
    (is (= (get (.getPoint solution) 1) 0.25))
    (is (= (.getValue solution) 0.75))))

