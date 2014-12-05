;; GENESIS

(ns quilts.sketch4
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn log [& s]
  (.log js/console (apply str s)))

(defn new-point [r]
  {:v 0.1
   :r r})

(defn setup []
  (q/frame-rate 30)
  {:points (map new-point (range 0 1 0.005))})

(defn tick [p]
  (let [speed 0.5]
    (update-in p [:v] #(+ % (* speed (:r p))))))

(defn update [state]
  (update-in state [:points] #(map tick %)))

(defn pulse [low high rate]
  (let [diff (- high low)
        half (/ diff 2)
        mid (+ low half)
        s (/ (q/millis) 1000.0)
        x (q/sin (* s (/ 1.0 rate)))]
    (+ mid (* x half))))

(defn get-x [p]
  (* 0.5 (q/width) (:r p) (q/cos (:v p))))

(defn get-y [p]
  (* 0.5 (q/height) (:r p) (q/sin (:v p))))

(defn draw [state]
  (q/background (pulse 10 30 1.0))
  (q/stroke 255 100)
  (q/with-translation [(* 0.5 (q/width)) (* 0.5 (q/height))]
    (loop [[p1 p2 & tail] (:points state)]
      (when (and p1 p2)
        (let [x1 (get-x p1)
              y1 (get-y p1)
              x2 (get-x p2)
              y2 (get-y p2)]
          (q/line x1 y1 x2 y2)
          (recur (cons p2 tail))))))
  )

(defn run-sketch-4 []
  (q/defsketch quilts
    :host "quilts"
    :size [600 600]
    :setup setup
    :update update
    :draw draw
    :middleware [m/fun-mode]))
