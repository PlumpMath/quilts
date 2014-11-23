(ns quilts.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [quilts.sketch1 :as s1]
            [quilts.sketch2 :as s2]))

(s2/run-sketch-2)
