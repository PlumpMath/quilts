;; DANCER

(ns quilts.sketch5
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def PI 3.14159265)
(def TWO_PI (* PI 2.0))

(defn log [& s]
  (.log js/console (apply str s)))

(defn pulse [low high rate]
  (let [diff (- high low)
        half (/ diff 2)
        mid (+ low half)
        s (/ (q/millis) 1000.0)
        x (q/sin (* s (/ 1.0 rate)))]
    (+ mid (* x half))))

(defn setup []
  (q/frame-rate 30)
  {})

(defn update [state]
  state)

(defn t []
  (* 0.001 (q/millis)))

(def win-width 800)
(def win-height 600)

(def x-max (/ win-width 4))
(def x-max-top (/ x-max 2))
(def y-max (/ win-height 2))
(def y-max-half (/ y-max 2))
(def y-max-half-third (/ y-max-half 3))
(def speed (* 0.5 (/ win-width 400)))

(defn stem [base-x]
  (let [magic 0.01
        x (+ base-x
             (pulse (- x-max-top) x-max-top 1.0))
        y (+ (- y-max)
             (* y-max-half (q/sin (+ (* speed (t)) (* magic base-x))))
             (* y-max-half-third (q/sin (* 2 (t)))))]
    (q/bezier base-x 0 base-x 0
              0 (- x-max) x y)))

(defn draw [state]
  (q/background (pulse 230 250 1.0))
  (q/stroke 0)
  (q/stroke-weight 1)
  (q/no-fill)
  (let [w (q/width)
        h (q/height)
        hw (/ w 2)
        hh (/ h 2)]
     (q/with-translation [hw h]
       (doseq [x (range (- x-max) x-max 2)]
         (stem x))))
  )

(defn run-sketch-5 []
  (q/defsketch quilts
    :host "quilts"
    :size [win-width win-height]
    :setup setup
    :update update
    :draw draw
    :middleware [m/fun-mode]))
