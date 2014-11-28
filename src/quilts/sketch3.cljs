;; Waves

(ns quilts.sketch3
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 30)
  {})

(defn update [state]
  {})

(def amp 100)

(defn t []
  (* 0.001 (q/millis)))

(defn calc-y [x mid]
  (+ mid (* (q/sin (+ (t) x)) amp)))

(defn wave [step mid-y]
  (doseq [x (range (- (q/width)) (q/width) step)]
    (let [next-x (+ x step)
          t (* x 0.01)
          next-t (* next-x 0.01)]
      (q/line x (calc-y t mid-y) next-x (calc-y next-t mid-y)))))

;; (defn p []
;;   (+ 10 (* 4 (q/sin (* 0.002 (q/millis))))))

(defn draw [state]
  (q/background 245)
  (q/fill 0 255 255)
  (let [move-down 200]
    (doseq [y (range move-down (+ amp (q/height)) 8)]
      (let [x-step (- (* y 0.8) move-down)]
        (wave x-step y))))
  )

(defn run-sketch-3 []
  (q/defsketch quilts
    :host "quilts"
    :size [1100 700]
    :setup setup
    :update update
    :draw draw
    :middleware [m/fun-mode]))
