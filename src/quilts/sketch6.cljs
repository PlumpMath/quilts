;; UNSTOPPABLE

(ns quilts.sketch6
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn log [& s]
  (.log js/console (apply str s)))

(defn setup []
  (q/frame-rate 100)
  (q/background 230)
  0)

(defn m []
  (let [mid (/ (q/width) 2)
        diff (- mid (q/mouse-x))
        a (q/abs diff)]
    (/ a mid)))

(defn update [state]
  (+ state (- 100.0 (* 99.0 (m)))))

(defn pulse [t low high rate]
  (let [diff (- high low)
        half (/ diff 2)
        mid (+ low half)
        s (/ t 1000.0)
        x (q/sin (* s (/ 1.0 rate)))]
    (+ mid (* x half))))

(defn draw [state]
  (q/no-stroke)
  (let [hw (/ (q/width) 2)
        hh (/ (q/height) 2)
        r (* 5.0 0.001 (q/millis))]
    (q/with-translation [hw hh]

      (q/fill (pulse state 100 200 1.0)
              (pulse state 100 200 2.0)
              (pulse state 100 200 3.0)
              100)

      (let [brush (+ 90.0 (* 20 (m)))]
        (q/with-rotation [r]
          (q/ellipse (pulse state 110 260 1.0) 0 brush brush)))
      )))

;; (q/mouse-x)

(defn run-sketch-6 []
  (q/defsketch quilts
    :host "quilts"
    :size [700 700]
    :setup setup
    :update update
    :draw draw
    :middleware [m/fun-mode]))
