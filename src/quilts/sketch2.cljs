(ns quilts.sketch2
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn create-ship []
  {:pos [30 100]})

(defn setup []
  (q/frame-rate 30)
  {:ship (create-ship)})

(defn move [[x y] dx dy]
  [(+ x dx) (+ y dy)])

(defn update [state]
  state)

(defn on-key [state]
  (let [k (q/key-as-keyword)]
    (.log js/console (str k))
    (case k
      :up (update-in state [:ship :pos] move 0 -5)
      :down (update-in state [:ship :pos] move 0 5)
      state)))

(defn draw-ship [self]
  (let [[x y] (:pos self)]
    (q/fill 0 0 0)
    (q/rect x y 10 10)))

(defn draw [state]
  (q/background 240)
  (q/line 0 0 (q/mouse-x) 100)
  (draw-ship (:ship state)))

(defn run-sketch-2 []
  (q/defsketch quilts
    :host "quilts"
    :size [220 220]
    :setup setup
    :update update
    :key-pressed on-key
    :draw draw
    :middleware [m/fun-mode]))
