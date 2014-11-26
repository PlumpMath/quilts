
(defn move [[x y] dx dy]
  [(+ x dx) (+ y dy)])

(defn ship-drift [ship]
  (let [[dx dy] (:speed ship)]
    (update-in ship [:pos] move dx dy)))

(defn mouse-input
  "Returns the mouse position scaled from the middle of the canvas.
   Returns nil if the mouse is outside."
  []
  (let [x (q/mouse-x)
        y (q/mouse-y)
        w (q/width)
        h (q/height)]
    (if (and (< 0 x w) (< 0 y h))
      [(- (/ x w) 0.5)
       (- (/ y h) 0.5)]
      nil)))

(defn scale-v2 [[x y] amount]
  [(* x amount)
   (* y amount)])

(defn mouse-control [state]
  (when-let [dir (scale-v2 (mouse-input) 5)]
    (.log js/console (str "dir: " dir))
    (assoc-in state [:ship :speed] dir)))

(defn update-drift [state]
  (update-in state [:ship] ship-drift))

(defn set-ship-speed [state dir]
  (assoc-in state [:ship :speed] dir))

(defn on-key [state]
  (let [k (q/key-code)
        strength 1]
    (.log js/console (str k))
    (case k
      38 (set-ship-speed state [0 (- strength)])
      40 (set-ship-speed state [0 strength])
      37 (set-ship-speed state [(- strength) 0])
      39 (set-ship-speed state [strength 0])
      state)))

(defn wrapped [diff n]
  (cond (< 200 diff) (+ n 400)
        (< diff -200) (- n 400)
        :else n))

(defn wrap-star [ship star]
  (let [[ship-x ship-y] (:pos ship)
        [star-x star-y] (:pos star)
        z (:z star)
        [star-screen-x star-screen-y] [(- star-x (* z ship-x)) (- star-y (* z ship-y))]
        dx (- ship-x star-x)
        dy (- ship-y star-y)
        new-pos [(wrapped dx star-x) (wrapped dy star-y)]]
    ;(log "diff: " [(int dx) (int dy)] " , star " (:pos star) " -> " new-pos)
    (assoc star :pos new-pos)))

