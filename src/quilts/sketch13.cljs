;; EYE

(ns quilts.sketch13
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn log [& s]
  (.log js/console (apply str s)))

(def PI 3.14159)
(def TWO-PI (* 2 PI))

(defn pulse [low high rate]
  (let [diff (- high low)
        half (/ diff 2)
        mid (+ low half)
        s (/ (q/millis) 1000.0)
        x (q/sin (* s (/ 1.0 rate)))]
    (+ mid (* x half))))

(defn setup []
  (q/frame-rate 100)
  (q/background 230)
  (q/rect-mode :center) )

(defn update [state]
  )

(defn draw [state]
  (q/no-stroke)
  (q/background 230 200 250)
  (q/stroke 255 50 100)
  (let [w (q/width)
        h (q/height)
        hw (/ w 2)
        hh (/ h 2)
        step (pulse 50.0 5.0 5.0)]
    (doseq [x (range 0 w step)]
      (q/line x 0 0 (- h x)))
    (doseq [x (range 0 w step)]
      (q/line 0 x x h))
    (doseq [x (range 0 w step)]
      (q/line w x (- h x) h))
    (doseq [x (range 0 w step)]
      (q/line w (- w x) (- h x) 0))
    (q/fill 255 50 100)
    (q/no-stroke)
    (q/ellipse hw hh (pulse 200 180 1.5) (pulse 180 200 1.5))))

(defn run-sketch-13 []
  (q/defsketch quilts
    :host "quilts"
    :size [500 500]
    :setup setup
    :update update
    :draw draw
    :middleware [m/fun-mode]))
