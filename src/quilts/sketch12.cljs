;; CHRISTMAS TREE

(ns quilts.sketch12
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

(def step 20)
(def half-step (/ step 2))

(defn setup []
  (q/frame-rate 100)
  (q/background 255)
  (q/rect-mode :center)
  )

(defn update [state]
    state)

(defn draw-tree-segment [size rot]
  (q/no-stroke)
  (q/with-rotation [rot]
    (q/fill 50 (pulse 180 230 0.2) 50)
    (q/triangle (- size) 0
                0 (- (* 1.2 size))
                size 0)
    (q/fill 200 0 0)
    ;; (doseq [[x y r] balls]
    ;;   (q/ellipse x y r r))
    ))

;; (defn get-balls [y]
;;   (let [i (int (/ y 70))]
;;     (case i
;;       4 [[-50 -20 20] [60 -20 15]]
;;       3 [[-50 0 30]]
;;       2 [[50 0 25] [80 0 15]]
;;       1 [[-70 0 20]]
;;       0 [[150 0 30]])))

(defn draw [state]
  (q/no-stroke)
  (q/background (pulse 60 130 1.0) 75 (q/random 80 100))
  (q/no-stroke)
  (let [w (q/width)
        h (q/height)
        hw (/ w 2)
        hh (/ h 2)]
    (q/with-translation [hw (- h 40)]
      (q/fill 60 50 10)
      (q/rect -10 0 20 60)
      (doseq [y (range 0 301 70)]
        (let [rot (pulse -0.07 0.07 2.0)]
          (q/push-matrix)
          (q/rotate rot)
          (q/with-translation [0 (- y)]
            (draw-tree-segment (- 200 (* y 0.3)) rot))))
      (doseq [n (range 6)]
        (q/pop-matrix)
        ))
    (when (< 98 (q/random 0 100))
      (q/fill 255)
      (q/rect hw hh 500 500))
    (q/stroke 0 100 200 50)
    (doseq [i (range 0 w 5)]
      (let [x (q/random 0 w)]
        (q/line x 0 x (q/random hh h))))
    )
  )

(defn run-sketch-12 []
  (q/defsketch quilts
    :host "quilts"
    :size [500 500]
    :setup setup
    :update update
    :draw draw
    :middleware [m/fun-mode]))
