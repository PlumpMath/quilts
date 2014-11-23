(ns quilts.sketch2
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 30)
  {})

(defn update [state]
  {})

(defn draw [state]
  (q/background 240)
  (q/fill 0 255 255)
  (q/ellipse (/ (q/width) 2) (/ (q/height) 2) 100 100))

(defn run-sketch-2 []
  (q/defsketch quilts
    :host "quilts"
    :size [500 500]
    :setup setup
    :update update
    :draw draw
    :middleware [m/fun-mode]))
