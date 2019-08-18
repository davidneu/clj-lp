(ns dev
  (:require
   [clojure.tools.namespace.repl :refer [refresh refresh-all clear]]   
   [clojure.java.io :as io]
   [clojure.java.javadoc :refer [javadoc]]
   [clojure.pprint :refer [pprint]]
   [clojure.repl :refer [apropos dir doc find-doc pst source]]
   [clojure.set :as set]
   [clojure.string :as str]
   
   [clj-mvt.tools]
   [clj-mvt.breakpoint :refer [break-on break-off]]))


(defn reset []
  (clj-mvt.tools/refresh))


(defn runit []
  (println "runit!"))

