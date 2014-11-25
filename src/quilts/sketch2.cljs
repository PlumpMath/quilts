(ns quilts.sketch2
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def universe-size 1000)
(def TWO-PI (* 2 3.14))

(defn create-ship []
  {:pos [(/ universe-size 2) (/ universe-size 2)]
   :dir 0.0
   :dir-change 0.0
   :speed 1.0
   :z 1.0
   :render-fn (fn [self] (q/rect 0 0 20 10))})

(defn rand-between [low high]
  (let [diff (- high low)]
    (+ low (rand diff))))

(defn star-render [star]
  (let [size (:size star)]
    (q/fill 255)
    (q/rect 0 0 size size)))

(defn create-star [pos]
  {:pos pos
   :dir (rand TWO-PI)
   :size (+ 1.0 (rand 3.0))
   :z (rand-between 0.2 0.5)
   :render-fn star-render})

(defn smoke-render [smoke]
  (let [age (:age smoke)
        size (/ 10.0 age)]
    (q/fill 255)
    (q/ellipse 0 0 size size)))

(defn create-smoke [pos]
  {:pos pos
   :dir 0.0
   :age 0.0
   :z 1.0
   :render-fn smoke-render})

(defn planet-render [planet]
  (let [size (:size planet)
        [r g b] (:color planet)]
    (q/fill r g b)
    (let [rs (:rs planet)
          step (/ TWO-PI (count rs))]
      (q/begin-shape)
      (doall (for [[a s] (map #(vector %1 %2)
                              (range 0 TWO-PI step)
                              rs)]
               (q/vertex (* size s (q/cos a))
                         (* size s (q/sin a)))))
      (q/end-shape))))

(defn create-planet [pos col]
  {:pos pos
   :dir (rand TWO-PI)
   :size (+ 50.0 (rand 20.0))
   :color col
   :z 1.0
   :rs (into [] (take (+ 5 (rand-int 5))
                      (repeatedly (fn [] (rand-between 0.7 1.0)))))
   :render-fn planet-render})

(defn random-star []
  (create-star [(rand universe-size) (rand universe-size)]))

(defn marie-star []
  (create-star [(rand-between 0 50) (rand-between 0 50)]))

(defn setup []
  (q/rect-mode :center)
  (q/frame-rate 30)
  {:ship (create-ship)
   :smoke []
   :stars (concat (take 100 (repeatedly random-star))
                  (take 10 (repeatedly marie-star)))
   :planets [(create-planet [200 200] [200 255 255])
             (create-planet [-300 0] [255 50 50])
             (create-planet [500 500] [50 250 150])]})

(defn translate-v2 [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn move-ship [ship]
  (let [speed (:speed ship)
        dir (:dir ship)
        dx (* speed (q/cos dir))
        dy (* speed (q/sin dir))]
    (update-in ship [:pos] translate-v2 [dx dy])))

(defn rotate-ship [ship]
  (let [dir-change (:dir-change ship)]
    (update-in ship [:dir] #(+ % dir-change))))

(defn wrapped [n]
  (cond (< universe-size n) 0.0
        (< n 0.0) universe-size
        :else n))

(defn wrap-ship [ship]
  (let [[x y] (:pos ship)
        new-pos [(wrapped x) (wrapped y)]]
    (assoc ship :pos new-pos)))

(defn update [state]
  (-> state
      (update-in [:ship] rotate-ship)
      (update-in [:ship] move-ship)
      (update-in [:ship] wrap-ship)
      ))

(defn faster [speed]
  (min 7.0 (+ speed 0.5)))

(defn slower [speed]
  (max 2.0 (- speed 0.5)))

(defn on-key-down [state]
  (let [k (q/key-code)]
    ;; (.log js/console (str k))
    (case k
      38 (update-in state [:ship :speed] faster)
      40 (update-in state [:ship :speed] slower)
      37 (assoc-in state [:ship :dir-change] -0.15)
      39 (assoc-in state [:ship :dir-change] 0.15)
      state)))

(defn on-key-up [state]
  (case (q/key-code)
    37 (assoc-in state [:ship :dir-change] 0)
    39 (assoc-in state [:ship :dir-change] 0)
    state))

  (defn draw-entity [entity [cam-x cam-y]]
  (let [[x y] (:pos entity)
        dir (:dir entity)
        z (:z entity)
        render-fn (:render-fn entity)]
    (q/fill 0 0 0)
    (do (q/push-matrix)
        (q/translate (- x (* z cam-x)) (- y (* z cam-y)))
        (q/rotate dir)
        (render-fn entity)
        (q/pop-matrix))))

(defn draw [state]
  (q/background (+ 50 (* (q/sin (/ (q/millis) 1000.0)) 50)) 100 140)
  (q/no-stroke)
  (let [ship-pos (-> state :ship :pos)
        cam-pos (translate-v2 ship-pos [(- (/ (q/width) 2))
                                        (- (/ (q/height) 2))])]
    (doseq [star (:stars state)]
      (draw-entity star cam-pos))
    (doseq [planet (:planets state)]
      (draw-entity planet cam-pos))
    (draw-entity (:ship state) cam-pos)
    (let [[x y] ship-pos]
      (q/text (str (int x) ", " (int y)) 20 20))
    ))

(defn run-sketch-2 []
  (q/defsketch quilts
    :host "quilts"
    :size [220 220]
    :setup setup
    :update update
    :key-pressed on-key-down
    :key-released on-key-up
    :draw draw
    :middleware [m/fun-mode]))

