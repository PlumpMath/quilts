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

(defn stem [base-x]
  (let [magic 0.01
        x (+ base-x
             (pulse -100 100 1.0)
             )
        y (+ -300
             (* 150 (q/sin (+ (* 1.0 (t)) (* magic base-x))))
             (* 50 (q/sin (* 2 (t)))))]
    (q/bezier base-x 0 base-x 0
              0 -200 x y)))

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
       (doseq [x (range -200 200 2)]
         (stem x))))
  )

(defn run-sketch-5 []
  (q/defsketch quilts
    :host "quilts"
    :size [800 600]
    :setup setup
    :update update
    :draw draw
    :middleware [m/fun-mode]))
