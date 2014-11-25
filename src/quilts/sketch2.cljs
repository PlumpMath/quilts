(ns quilts.sketch2
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def TWO-PI (* 2 3.14))

(defn create-ship []
  {:pos [30 100]
   :dir 0.0
   :speed 1.0
   :z 1.0})

(defn rand-between [low high]
  (let [diff (- high low)]
    (+ low (rand diff))))

(defn create-star [pos]
  {:pos pos
   :dir (rand TWO-PI)
   :size (+ 1.0 (rand 3.0))
   :z (rand-between 0.2 0.5)})

(defn create-planet [pos col]
  {:pos pos
   :dir (rand TWO-PI)
   :size (+ 50.0 (rand 200.0))
   :color col
   :z (rand-between 1.3 1.5)})

(defn random-star []
  (create-star [(rand-int 1000) (rand-int 1000)]))

(defn marie-star []
  (create-star [(rand-between -50 10) (rand-between -50 10)]))

(defn setup []
  (q/rect-mode :center)
  (q/frame-rate 30)
  {:ship (create-ship)
   :stars (concat (take 100 (repeatedly random-star))
                  (take 10 (repeatedly marie-star)))
   :planets [(create-planet [200 200] [200 255 255])
             (create-planet [-300 0] [255 50 50])]})

(defn translate-v2 [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn move-ship [ship]
  (let [speed (:speed ship)
        dir (:dir ship)
        dx (* speed (q/cos dir))
        dy (* speed (q/sin dir))]
    (update-in ship [:pos] translate-v2 [dx dy])))

(defn update [state]
  (update-in state [:ship] move-ship))

(defn faster [speed]
  (min 10.0 (+ speed 0.5)))

(defn slower [speed]
  (max 1.0 (- speed 0.5)))

(defn on-key [state]
  (when-let [k (q/key-code)]
    ;; (.log js/console (str k))
    (case k
      38 (update-in state [:ship :speed] faster)
      40 (update-in state [:ship :speed] slower)
      37 (update-in state [:ship :dir] #(- % 0.1))
      39 (update-in state [:ship :dir] #(+ % 0.1))
      state)))

(defn draw-entity [entity [cam-x cam-y] render-fn]
  (let [[x y] (:pos entity render-fn)
        dir (:dir entity render-fn)
        z (:z entity)]
    (q/fill 0 0 0)
    (do (q/push-matrix)
        (q/translate (- x (* z cam-x)) (- y (* z cam-y)))
        (q/rotate dir)
        (render-fn entity)
        (q/pop-matrix))))

(defn star-render [star]
  (let [size (:size star)]
    (q/fill 255)
    (q/rect 0 0 size size)))

(defn planet-render [planet]
  (let [size (:size planet)
        [r g b] (:color planet)]
    (q/fill r g b)
    (q/ellipse 0 0 size size)))

(defn draw [state]
  (q/background 50 100 140)
  (q/no-stroke)
  (let [ship-pos (-> state :ship :pos)
        cam-pos (translate-v2 ship-pos [(- (/ (q/width) 2))
                                        (- (/ (q/height) 2))])]
    (doseq [star (:stars state)]
      (draw-entity star cam-pos star-render))
    (doseq [planet (:planets state)]
      (draw-entity planet cam-pos planet-render))
    (draw-entity (:ship state) cam-pos (fn [self] (q/rect 0 0 20 10)))
    ))

(defn run-sketch-2 []
  (q/defsketch quilts
    :host "quilts"
    :size [220 220]
    :setup setup
    :update update
    :key-pressed on-key
    :draw draw
    :middleware [m/fun-mode]))
