;; Dry Paint

(ns quilts.sketch11
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

(defn lamp [x y]
  {:x x
   :y y
   :col [300 300 300]})

(def step 20)
(def half-step (/ step 2))

(defn setup []
  (q/frame-rate 100)
  (q/background 255)
  (q/rect-mode :center)
  (into [] (for [y (range 0 (q/height) step)
                 x (range 0 (q/width) step)]
             (lamp x y))))

(defn get-lamp-index [[x y]]
  (let [width-count (/ (q/width) step)
        index (+ (* y width-count) x)]
    ;;(log "x: " x ", y: " y ", index: " index)
    (if (and (< 0 (q/mouse-x) (q/width))
             (< 0 (q/mouse-y) (q/height)))
      index
      nil)))

(defn mouse-coords []
  [(int (/ (q/mouse-x) step))
   (int (/ (q/mouse-y) step))])

(defn update [state]
  (if-let [index (get-lamp-index (mouse-coords))]
    (assoc-in state [index :col] [(pulse 0 255 3.0)
                                  (pulse 0 255 5.0)
                                  (pulse 0 255 7.0)])
    state))

(defn draw [state]
  (q/no-stroke)
  (q/background 255)
  (q/no-stroke)
  (let [w (q/width)
        h (q/height)
        hw (/ w 2)
        hh (/ h 2)]
    (doseq [lamp state]
      (let [x (:x lamp)
            y (:y lamp)
            col (:col lamp)]
        (apply q/fill (map + col (repeat (* 5 (q/sin (+ (* 0.01 x) (* 0.01 y) (* 0.01 (q/millis))))))))
        (q/rect (+ half-step x) (+ half-step y) step step))
      )))

(defn run-sketch-11 []
  (q/defsketch quilts
    :host "quilts"
    :size [500 500]
    :setup setup
    :update update
    :draw draw
    :middleware [m/fun-mode]))
