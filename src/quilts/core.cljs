(ns quilts.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [quilts.sketch1 :as s1]
            [quilts.sketch2 :as s2]
            [quilts.sketch3 :as s3]))

(s3/run-sketch-3)
