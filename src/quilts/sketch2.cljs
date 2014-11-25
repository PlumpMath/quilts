(ns quilts.sketch2
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn create-ship []
  {:pos [30 100]
   :dir 0.0
   :speed 2.0})

(defn setup []
  (q/rect-mode :center)
  (q/frame-rate 30)
  {:ship (create-ship)})

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

(defn draw-entity [entity render-fn]
  (let [[x y] (:pos entity render-fn)
        dir (:dir entity render-fn)]
    (q/fill 0 0 0)
    (do (q/push-matrix)
        (q/translate x y)
        (q/rotate dir)
        (render-fn)
        (q/pop-matrix))))

(defn draw [state]
  (q/background 0 100 240)
  (draw-entity (:ship state) #(q/rect 0 0 20 10)))

(defn run-sketch-2 []
  (q/defsketch quilts
    :host "quilts"
    :size [220 220]
    :setup setup
    :update update
    :key-pressed on-key
    :draw draw
    :middleware [m/fun-mode]))
