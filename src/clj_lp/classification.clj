(ns clj-lp.classification
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clj-du.core :as du]            
            [mvt-clj.breakpoint :refer [break]]
            [clj-lp.core :as lp]))


;; From https://clojuredocs.org/clojure.core/reduce-kv
(defn update-map [m f] 
  (reduce-kv (fn [m k v] 
    (assoc m k (f v))) {} m))


(def breast-cancer-wisconsin-csv-filename
  "~/projects/welp-resources/wisconsin-breast-cancer/breast-cancer-wisconsin.csv")


(defn read-breast-cancer-wisconsin-csv [filename]
  (->> (du/read-csv-to-hash-map filename)
       (map #(update-map % (fn [x] (try (Integer/parseInt x) (catch Exception e nil)))))
       (filter (fn [m] (not (some nil? (vals m)))))
       (map #(update % :class (fn [x] (cond (= x 2) 0 (= x 4) 1))))
       (map #(dissoc % :sample-code-number))
       (group-by :class)))


;; TO DO: Rmove hard coding of epsilon, max-ulps, and cut-off
(defn model-1 [t f]
  (let [variables (sort (vec (set (into (flatten (map keys t)) (flatten (map keys f))))))

        objective-function
        {:linear-terms (into {} (for [x (range (count t)) y (range (count f))] (vector (keyword (str "z_" x "_" y)) 1)))
         :constant-term 0}

        convex-combination-constraint [{:lhs (into {} (map (fn [variable] [variable 1]) variables)) :rel '= :rhs 1}]
        
        non-negative-constraints (for [x (range (count t)) y (range (count f))] {:lhs {(keyword (str "z_" x "_" y)) 1} :rel '>= :rhs 0})

        lower-bound-constraints (map (fn [variable] {:lhs {variable 1} :rel '>= :rhs 0}) variables)
        
        upper-bound-constraints (map (fn [variable] {:lhs {variable 1} :rel '<= :rhs 1}) variables)
        
        t-f-constraints
        (for [x (range (count t)) y (range (count f))]
          {:lhs (into {(keyword (str "z_" x "_" y)) -1} (map (fn [variable] {variable (- (get (get f y) variable 0) (get (get t x) variable 0))}) variables))
           :rel '<=
           :rhs 0})

        ;; constraints (concat convex-combination-constraint non-negative-constraints lower-bound-constraints upper-bound-constraints t-f-constraints)

        ;; Note: Not including non-negative-constraints and lower-bound-constraints because using NonNegativeConstraint results in a substantial speed increase.
        constraints (concat convex-combination-constraint upper-bound-constraints t-f-constraints)]

    ;; (minimize objective-function constraints {:epsilon 0.01 :max-ulps 30 :cut-off 1.0e-2})
    (lp/minimize objective-function constraints {:non-negative-constraint true :epsilon 0.01 :max-ulps 30 :cut-off 1.0e-2})))


(defn auc-plus [a b c d]
  (/ (+ (* a b) (* 2 a d) (* c d))
     (* 2 (+ a c) (+ b d))))


(defn auc-minus [a b c d]
  (/ (+ (* a b) (* 2 b c) (* c d))
     (* 2 (+ a c) (+ b d))))


(defn auc [a b c d]
  (max (auc-plus a b c d) (auc-minus a b c d)))


(defn model-1-classifier [data]
  (let [card-t 100

        t (shuffle (get data 1))

        f (shuffle (get data 0))

        ;; Using stratified cross-validation, i.e. the number of true and false examples in each fold is equal.
        t-train (take card-t t)
        t-test (take card-t (drop card-t t))
        f-train (take card-t f)
        f-test (take card-t (drop card-t f))

        all-z (model-1
               (vec (map #(dissoc % :class) t-train))
               (vec (map #(dissoc % :class) f-train)))
        
        ;; TO DD: Really should maintain a list of the original decision variables in case one of them begins with z_
        z (into {} (filter (fn [[k v]] (not (str/starts-with? (name k) "z_"))) all-z))

        ;; real-valued function
        ;; rfn closes over z
        rfn
        (let [z z]
          (fn [x]
            (reduce
             +
             (map
              (fn [[k v]] (* (get z k 0.0) v))
              x))))

        train (reverse (sort-by first (map (fn [x] [(rfn x) (:class x)]) (into t-train f-train))))
        
        ;; calculate auc for each threshold,
        thresholds
        (map
         (fn [{:keys [a b c d] :as all}]
           (assoc all :auc (auc a b c d)))
         (map
          (fn [[threshold _]]
            (assoc
             (reduce
              (fn [abcd [value class]]
                (cond (and (>= value threshold) (= class 1))
                      (update abcd :a inc)

                      (and (>= value threshold) (= class 0))
                      (update abcd :b inc)

                      (and (< value threshold) (= class 1))
                      (update abcd :c inc)

                      (and (< value threshold) (= class 0))
                      (update abcd :d inc)

                      :else
                      (throw (Exception. "Unexpected abcd"))))
              {:a 0 :b 0 :c 0 :d 0}
              train)
             :threshold threshold))
          train))

        ;; pick threshold with largest auc
        threshold (first (reverse (sort-by :auc thresholds)))

        ;; boolean-valued function - returns true if >= threshold and false otherwise
        ;; bfn closes over rfn and threshold
        bfn
        (let [rfn rfn
              threshold (:threshold threshold)]
          (fn [x] (if (>= (rfn x) threshold) 1 0)))
        
        classifier {:bfn bfn
                    :rfn rfn
                    :threshold (:threshold threshold)}

        test (reverse (sort-by first (map (fn [x] [(rfn x) (bfn x) (:class x)]) (into t-test f-test))))]

    ;; TO DO: Add an option to specify a results directory to write files like this

    ;; To see the performance of the model on the training set:
    ;; In Clojure:
    ;; > (du/write-hash-maps-to-csv "/tmp/roc-training.csv" "response,value" training)
    ;; In R:
    ;; > library(pROC)
    ;; > rt <- read.csv("/tmp/roc-training.csv")
    ;; > roc(rt, "value", "response")
    ;; > plot(roc(rt, "value", "response"))

    {:classifier classifier :train train :test test :card card-t}))


(comment

  (def bcw (read-breast-cancer-wisconsin-csv breast-cancer-wisconsin-csv-filename))

  (def bcw-classifier (clj-lp.core/model-1-classifier bcw))

  )

