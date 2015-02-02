;; HYPER

(ns quilts.sketch10
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
  (q/background 255)
  (q/rect-mode :center)
  {:r 0.0
   :col 0}
  )

;; {:pos [(* 0.5 (q/width)) (* 0.5 (q/height))]
;;  :dir [(q/sin (q/millis)) (q/cos (q/millis))]}

;; (defn add-pos [pos1 pos2]
;;   (into [] (map + pos1 pos2)))

;; (defn mul-pos [pos x]
;;   (into [] (map * pos (repeat x))))

(defn tick [state]
  (update-in state [:r] #(+ % 5.0)))

(defn flip [state]
  {:r 0.0
   :col (if (= 0 (:col state)) 255 0)})

(defn update [state]
  (if (< (:r state) 300)
    (tick state)
    (flip state)))

(defn draw [state]
  (q/stroke (:col state))
  (let [hw (* 0.5 (q/width))
        hh (* 0.5 (q/height))]
    (doseq [x (range 0 50)]
      (let [rand-ang (q/random 0 TWO-PI)
            r (:r state)]
        (q/line hh
                hw
                (+ hh (* (q/sin rand-ang) r))
                (+ hw (* (q/cos rand-ang) r))))))
  )

(defn run-sketch-10 []
  (q/defsketch quilts
    :host "quilts"
    :size [400 400]
    :setup setup
    :update update
    :draw draw
    :middleware [m/fun-mode]))
