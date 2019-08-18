(ns clj-lp.core
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [mvt-clj.breakpoint :refer [break]])
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


(defn linear-objective-function [lof xs]
  (LinearObjectiveFunction.
   (double-array (count xs) (mapv (fn [x] (double (get (:linear-terms lof) x 0.0))) xs))
   (double (:constant-term lof))))


;; TO DO: Add ability to use LinearConstraint. variant with variables on both lhs and rhs
(defn linear-constraint [lcm xs]
  (LinearConstraint.
   (double-array (count xs) (mapv (fn [x] (double (get (:lhs lcm) x 0.0))) xs))
   (case (str (:rel lcm))
     ">=" Relationship/GEQ
     "<=" Relationship/LEQ
     "=" Relationship/EQ)
   (double (:rhs lcm))))


(defn linear-constraint-set [lcv xs]
  (LinearConstraintSet.
   (mapv (fn [lc] (linear-constraint lc xs)) lcv)))


;; Default convergence criteria:
;; epsilon - Amount of error to accept for algorithm convergence (double).
;; maxUlps - Amount of error to accept in floating point comparisons (int).
;; cutOff - Values smaller than the cutOff are treated as zero (double).
(def default-options
  {:non-negative-constraint false
   :epsilon 1.0e-6  
   :max-ulps 10
   :cut-off 1.0e-10})


;; TO DO: Consider supporting the ability to pass in the following combinations of epsilon, maxUlps, cutOff into optimize
;; SimplexSolver()
;; SimplexSolver(double epsilon)
;; SimplexSolver(double epsilon, int maxUlps)
;; SimplexSolver(double epsilon, int maxUlps, double cutOff)
(defn- optimize
  ([lofm lcv gt]
   (optimize lofm lcv gt default-options))
  ([lofm lcv gt {:keys [non-negative-constraint epsilon max-ulps cut-off]
                 :or {non-negative-constraint (:non-negative-constraint default-options)
                      epsilon (:epsilon default-options)
                      max-ulps (:max-ulps default-options)
                      cut-off (:cut-off default-options)}}]
   ;; (println "non-negative-constraint =" non-negative-constraint)
   ;; (println "epsilon =" epsilon)
   ;; (println "max-ulps =" max-ulps)
   ;; (println "cut-off =" cut-off)
   (let [xs (into (apply sorted-set ((comp keys :linear-terms) lofm)) (flatten (map (comp keys :lhs) lcv)))
         lof (linear-objective-function lofm xs)
         lcs (linear-constraint-set lcv xs)
         nnc (NonNegativeConstraint. non-negative-constraint)
         ;; optimization-data (into-array OptimizationData [lof lcs gt])
         optimization-data (into-array OptimizationData [lof lcs gt nnc])]
     (try
       ;; (println  (.isRestrictedToNonNegative (SimplexSolver. epsilon max-ulps cut-off)))
       (let [solution (.getPoint (.optimize (SimplexSolver. epsilon max-ulps cut-off) optimization-data))]
         ;; [solution (.getPoint (.optimize (SimplexSolver.) optimization-data))]
         (into {} (map-indexed (fn [i x] [x (get solution i)]) xs)))
       (catch UnboundedSolutionException e
         (throw
          (ex-info "UnboundedSolutionException" {:xs xs :lof lof :lcs lcs})))
       (catch NoFeasibleSolutionException e
         (throw
          (ex-info "NoFeasibleSolutionException" {:xs xs :lof lof :lcs lcs})))))))


(defn maximize
  ([lofm lcv]
   (maximize lofm lcv default-options))
  ([lofm lcv options]
   (optimize lofm lcv GoalType/MAXIMIZE options)))


(defn minimize
  ([lofm lcv]
   (minimize lofm lcv default-options))
  ([lofm lcv options]
   (optimize lofm lcv GoalType/MINIMIZE options)))

