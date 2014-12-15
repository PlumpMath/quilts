;; EMERALD

(ns quilts.sketch7
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def PI 3.14159)
(def TWO-PI (* 2 PI))

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
  (q/frame-rate 100)
  (q/background 230)
  )

(defn update [state]
  )

(defn draw [state]
  (q/no-stroke)
  (q/background 255)

  (let [w (q/width)
        h (q/height)
        hw (/ w 2)
        hh (/ h 2)]
    
    (q/fill (pulse 20 50 2.0) 230 (pulse 150 200 1.0))
    (q/ellipse hw hh w h)

    (q/stroke 255 255 255 100)

    (q/with-translation [hw hh]
      (doseq [a (range 0 TWO-PI (/ PI 256.0))]
        (let [inner-r (* hw 0.5)
              outer-r hw
              skew1 (* 0.001 (q/millis) a) ;; (pulse (- (* a 5.0)) (* a 5.0) 2.0)
              skew2 (* skew1 2.0)]
          (q/line (* inner-r (q/cos (+ skew1 a)))
                  (* inner-r (q/sin (+ skew1 a)))
                  (* outer-r (q/cos (+ skew2 a)))
                  (* outer-r (q/sin (+ skew2 a)))))
        ))))

(defn run-sketch-7 []
  (q/defsketch quilts
    :host "quilts"
    :size [400 400]
    :setup setup
    :update update
    :draw draw
    :middleware [m/fun-mode]))
