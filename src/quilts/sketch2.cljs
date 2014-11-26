(ns quilts.sketch2
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def PI 3.14159)
(def TWO-PI (* 2 PI))

(defn log [& args]
  (.log js/console (apply str args)))

(defn pulse [low high rate]
  (let [diff (- high low)
        half (/ diff 2)
        mid (+ low half)
        s (/ (q/millis) 1000.0)
        x (q/sin (* s (/ 1.0 rate)))]
    (+ mid (* x half))))

(defn rand-between [low high]
  (let [diff (- high low)]
    (+ low (rand diff))))

(defn rand-coord [size]
  [(rand-between (- size) size)
   (rand-between (- size) size)])

(defn translate-v2 [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn render-ship [ship]
  (q/fill 50 80 50)
  (q/rect -2 0 5 14)
  (q/fill 150 180 150)
  (q/triangle 0 -10 25 0 0  10)
  (q/fill 30 100 30)
  (q/ellipse 8 0 8 8))

(defn create-ship []
  {:pos [-1000 1000]
   :dir -0.4
   :dir-change 0.0
   :speed 0.1
   :z 1.0
   :render-fn render-ship})

(defn render-star [star]
  (let [size (:size star)]
    (q/fill 255)
    (q/rect 0 0 size size)))

(defn create-star [pos]
  {:pos pos
   :dir (rand TWO-PI)
   :size (+ 1.0 (rand 3.0))
   :z (rand-between 0.2 0.7)
   :render-fn render-star})

(defn random-star []
  (create-star (rand-coord 1000)))

(defn render-smoke [smoke]
  (let [age (:age smoke)
        size (max 0.0 (- 10.0 (* 5.0 age)))
        [r g b] (:col smoke)]
    (q/fill r g b 200)
    (q/ellipse 0 0 size size)))

(defn create-smoke [[x y]]
  {:pos [(+ x (rand-between -3 3))
         (+ y (rand-between -3 3))]
   :dir 0.0
   :age 0.0
   :z 1.0
   :col [(rand-between 150 255)
         (rand-between 100 200)
         (rand-between 0 100)]
   :render-fn render-smoke})

(defn render-planet [planet]
  (let [size (:size planet)
        [r g b] (:color planet)]
    (q/fill r g b)
    (let [rs (:rs planet)
          step (/ TWO-PI (count rs))]
      (q/begin-shape)
      (doall (for [[angle radius] (map #(vector %1 %2) (range 0 TWO-PI step) rs)]
               (q/vertex (* size radius (q/cos angle))
                         (* size radius (q/sin angle)))))
      (q/end-shape))))

(defn generate-radiuses []
  (into [] (take (+ 5 (rand-int 5))
                 (repeatedly #(rand-between 0.7 1.0)))))

(defn create-planet [pos color]
  {:pos pos
   :dir (rand TWO-PI)
   :dir-change (rand-between -0.01 0.01)
   :size (+ 50.0 (rand 50.0))
   :drift [(rand-between -0.3 0.3) (rand-between -0.3 0.3)]
   :color color
   :z 1.0
   :rs (generate-radiuses)
   :render-fn render-planet})

(defn random-planet []
  (create-planet (rand-coord 1000)
                 [(rand-between 0 255)
                  (rand-between 50 150)
                  (rand-between 50 150)]))

(defn setup []
  (q/rect-mode :center)
  (q/frame-rate 30)
  {:ship (create-ship)
   :smoke []
   :stars (take 3000 (repeatedly random-star))
   :planets (take 50 (repeatedly random-planet))})

(defn move-ship [ship]
  (let [speed (+ 1.0 (* 7.0 (:speed ship)))
        dir (:dir ship)
        dx (* speed (q/cos dir))
        dy (* speed (q/sin dir))]
    (update-in ship [:pos] translate-v2 [dx dy])))

(defn auto-rotate [entity]
  (let [dir-change (:dir-change entity)]
    (update-in entity [:dir] #(+ % dir-change))))

(defn wiggle-ship [ship]
  (let [speed (:speed ship)
        a (+ 0.01 (* 0.02 speed))]
    (update-in ship [:dir] #(+ % (pulse (- a) a 0.1)))))

(defn drift-planet [planet]
  (let [[dx dy] (:drift planet)]
    (update-in planet [:pos] translate-v2 [dx dy])))

(defn emit-smoke [state]
  (let [speed (-> state :ship :speed)]
    (if (< (rand) (+ 0.2 speed))
      (let [ship-pos (-> state :ship :pos)]
        (update-in state [:smoke] conj (create-smoke ship-pos)))
      state)))

(defn age-smoke [smoke]
  (update-in smoke [:age] #(+ % 0.033)))

(defn is-old? [smoke]
  (< 3.0 (:age smoke)))

(defn remove-old-smokes [smokes]
  (remove is-old? smokes))

(defn update [state]
  (-> state
      (update-in [:ship] auto-rotate)
      (update-in [:ship] wiggle-ship)
      (update-in [:ship] move-ship)
      emit-smoke
      (update-in [:smoke] (fn [smokes] (map age-smoke smokes)))
      (update-in [:smoke] remove-old-smokes)
      (update-in [:planets] #(map auto-rotate %))
      (update-in [:planets] #(map drift-planet %))))

(defn faster [speed]
  (min 1.0 (+ speed 0.25)))

(defn slower [speed]
  (max 0.0 (- speed 0.25)))

(defn on-key-down [state]
  (let [k (q/key-code)]
    (cond
     (or (= k 87) (= k 38)) (update-in state [:ship :speed] faster)
     (or (= k 83) (= k 40)) (update-in state [:ship :speed] slower)
     (or (= k 65) (= k 37)) (assoc-in state [:ship :dir-change] -0.15)
     (or (= k 68) (= k 39)) (assoc-in state [:ship :dir-change] 0.15)
     :else state)))

(defn on-key-up [state]
  (let [k (q/key-code)]
    (if (or (= k 37) (= k 39) (= k 65) (= k 68))
      (assoc-in state [:ship :dir-change] 0)
      state)))

(defn on-screen? [x y]
  (let [margin 100]
    (and (<= (- margin) x (+ margin (q/width)))
         (<= (- margin) y (+ margin (q/height))))))

(defn draw-entity [entity [cam-x cam-y]]
  (let [[x y] (:pos entity)
        dir (:dir entity)
        z (:z entity)
        render-fn (:render-fn entity)
        screen-x (- x (* z cam-x))
        screen-y (- y (* z cam-y))]
    (when (on-screen? screen-x screen-y)
      (do (q/push-matrix)
          (q/translate screen-x screen-y)
          (q/rotate dir)
          (render-fn entity)
          (q/pop-matrix)))))

(defn draw [state]
  (q/background (pulse 20 40  15.0)
                (pulse 40 60 40.0)
                (pulse 50 70 5.0))
  (q/no-stroke)
  (let [ship-pos (-> state :ship :pos)
        cam-pos (translate-v2 ship-pos [(- (/ (q/width) 2))
                                        (- (/ (q/height) 2))])]
    (doseq [star (:stars state)]
      (draw-entity star cam-pos))
    (doseq [planet (:planets state)]
      (draw-entity planet cam-pos))
    (doseq [smoke (:smoke state)]
      (draw-entity smoke cam-pos))
    (draw-entity (:ship state) cam-pos)
    #_(let [[x y] ship-pos]
      (q/fill 255 20)
      (q/text (str (int x) " : " (int y)) 5 15))
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

