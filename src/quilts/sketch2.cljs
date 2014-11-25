(ns quilts.sketch2
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn create-ship []
  {:pos [30 100]
   :dir 0.0
   :speed 0.0})

(defn create-star [pos]
  {:pos pos
   :dir 0.0})

(defn setup []
  (q/rect-mode :center)
  (q/frame-rate 30)
  {:ship (create-ship)
   :stars [(create-star [50 50])
           (create-star [60 50])
           (create-star [70 50])]})

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
  (min 4.0 (+ speed 0.5)))

(defn slower [speed]
  (max 0.5 (- speed 0.5)))

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
        dir (:dir entity render-fn)]
    (q/fill 0 0 0)
    (do (q/push-matrix)
        (q/translate (- x cam-x) (- y cam-y))
        (q/rotate dir)
        (render-fn)
        (q/pop-matrix))))

(defn draw [state]
  (q/background 50 100 140)
  (let [ship-pos (-> state :ship :pos)
        cam-pos (translate-v2 ship-pos [(- (/ (q/width) 2))
                                        (- (/ (q/height) 2))])]
    (draw-entity (:ship state) cam-pos #(q/rect 0 0 20 10))
    (doseq [star (:stars state)]
      (draw-entity star cam-pos (fn [] (q/rect 0 0 3 3))))))

(defn run-sketch-2 []
  (q/defsketch quilts
    :host "quilts"
    :size [220 220]
    :setup setup
    :update update
    :key-pressed on-key
    :draw draw
    :middleware [m/fun-mode]))
