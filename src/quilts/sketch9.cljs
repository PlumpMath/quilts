;; STARS

(ns quilts.sketch9
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn log [& s]
  (.log js/console (apply str s)))

(def PI 3.14159)
(def TWO-PI (* 2 PI))

(defn star []
  (let [x (q/random 0 (q/width))
        y (q/random 0 (q/height))]
    {:x x
     :y y
     :size (+ 1.0 (* 2.0 (q/noise x y))) ;; (q/random 2.0 5.0)
     :rot (q/random TWO-PI)}))

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
  (q/rect-mode :center)
  (take 300 (repeatedly star))
  )

(defn rotate [star]
  (let [x (:x star)]
    (update-in star [:rot] (partial + (* 0.2 (q/sin x))))))

(defn update [state]
  (map rotate state))

(defn draw [state]
  (q/no-stroke)
  (q/background (pulse 20 50 2.0) 70 (pulse 80 100 1.0))
  (q/fill 255)
  (doseq [s state]
    (let [x (:x s)
          y (:y s)
          size (* (pulse 0.5 1.5 (+ 0.5 (* 0.5 (q/abs (q/sin x)))))
                  (:size s))
          rot (:rot s)]
      (q/fill 255 (pulse 5 50 (q/noise x y)))
      (q/with-translation [x y]
        (q/with-rotation [rot]
          (q/rect 0 0 size size)))
      ))
  )

(defn run-sketch-9 []
  (q/defsketch quilts
    :host "quilts"
    :size [1200 400]
    :setup setup
    :update update
    :draw draw
    :middleware [m/fun-mode]))
