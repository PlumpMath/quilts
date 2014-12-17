;; TRAIN WINDOW

(ns quilts.sketch7
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn log [& s]
  (.log js/console (apply str s)))

(defn pulse [low high rate]
  (let [diff (- high low)
        half (/ diff 2)
        mid (+ low half)
        s (/ (q/millis) 1000.0)
        x (q/sin (* s (/ 1.0 rate)))]
    (+ mid (* x half))))

(defn make-tree [x]
  {:x x
   :y (q/random 100 120)})

(defn make-field [x]
  {:x x
   :y (q/random 170 180)})

(defn setup []
  (q/frame-rate 100)
  (q/background 230)
  {:trees (into [] (map make-tree (range -10 (+ 10 (q/width)) 10)))
   :fields (into [] (map make-field (range -20 (+ 20 (q/width)) 20)))
   :lights [{:x 500 :y (q/random 300 400)}]})

(defn wrap [margin tree]
  (let [{x :x y :y} tree]
    {:x (if (< x (- margin)) (+ margin (q/width)) x)
     :y y}))

(defn move-tree [speed tree]
  (-> tree
      (update-in [:x] #(- % speed))
      (#(wrap 10 %))))

(defn move-field [speed field]
  (-> field
      (update-in [:x] #(- % speed))
      (#(wrap 20 %))))

(defn wrap-light [tree]
  (let [{x :x y :y} tree]
    {:x (if (< x -20) (+ (q/width) (q/random 100 300)) x)
     :y (if (< x -20) (q/random 200 450) y)}))

(defn move-light [light]
  (-> light
      (update-in [:x] #(- % 1.0))
      wrap-light))

(defn update [state]
  (-> state
      (update-in [:trees] #(map (partial move-tree 0.2) %))
      (update-in [:fields] #(map (partial move-field 0.3) %))
      (update-in [:lights] #(map (partial move-light) %))))

(defn draw-horizon [objs]
  (q/begin-shape)
  (doseq [tree (sort-by :x objs)]
    (let [x (:x tree)
          y (:y tree)]
      (q/vertex x y)))
  (q/vertex (q/width) (q/height))
  (q/vertex 0 (q/height))  
  (q/end-shape))

(defn draw-lights [lights]
  (doseq [light lights]
    (let [x (:x light)
          y (:y light)
          r (q/random 10 30)]
      (q/ellipse x y r r))))

(defn draw [state]
  (q/no-stroke)
  (q/background (pulse 20 50 2.0) 30 (pulse 75 175 1.0))
  (q/no-stroke)
  (q/fill 20 (pulse 10 30 1.0) 20)
  (draw-horizon (:trees state))
  (q/fill (pulse 20 35 5.0) (pulse 30 45 2.0) 20)
  (draw-horizon (:fields state))
  (q/fill (q/random 200 255) 100 10)
  (draw-lights (:lights state))
  )

(defn run-sketch-7 []
  (q/defsketch quilts
    :host "quilts"
    :size [300 500]
    :setup setup
    :update update
    :draw draw
    :middleware [m/fun-mode]))
