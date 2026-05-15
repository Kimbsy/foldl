(ns foldl.scenes.level-01
  (:require [clunk.core :as c]
            [clunk.input :as i]
            [clunk.palette :as p]
            [clunk.sprite :as sprite]
            [clunk.util :as u]
            [clunk.shape :as shape]
            [clunk.collision :as collision]))

(def coral-pink (p/hex->rgba "#FF9B85"))

(defn sprites
  "The initial list of sprites for this scene"
  [{:keys [window] :as state}]
  [])

(defn line-intersection-point
  "Make sure the lines intersect using `collision/lines-intersect?`
  before calling this to avoid division by zero."
  [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (let [t (/ (- (* (- x1 x3) (- y3 y4))
                (* (- y1 y3) (- x3 x4)))
             (- (* (- x1 x2) (- y3 y4))
                (* (- y1 y2) (- x3 x4))))]
    [(+ x1 (* t (- x2 x1)))
     (+ y1 (* t (- y2 y1)))]))

(defn reflect
  [[px py :as p] [[x1 y1] [x2 y2] :as line]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]

    ;; avoid dividing by zero when lines are vertical or horizontal
    (cond
      (zero? dx)
      [(+ px (* 2 (- x1 px))) py]

      (zero? dy)
      [px (+ py (* 2 (- y1 py)))]

      :else
      (let [m ( if (zero? dx)
               ##Inf
               (/ dy dx))
            c (- y2 (* m x2))

            ;; the ratio perpendicular to line
            m-perp (- (/ 1 m))

            ;; p is on the perp line, so
            c-perp (- py (* m-perp px))

            ;; intersection of line and perp-line
            ;; m*x + c = m-perp*x + c-perp
            ;; m*x = m-perp*x + c-perp - c
            ;; m*x - m-perp*x = c-perp - c
            ;; x*(m - m-perp) = c-perp - c
            ;; x = (c-perp - c) / (m - m-perp)
            ix (/ (- c-perp c)
                  (- m m-perp))
            iy (+ (* m ix) c)

            ;; reflected point is double x to intersect
            rx (+ ix (- ix px))
            ry (+ iy (- iy py))]

        [(float rx) (float ry)]))))

(defn project-fold-line
  [state l-start l-end]
  (let [d (* 2 (apply + (u/window-size (:window state))))
        line (mapv - l-end l-start)
        ul (u/unit-vector line)
        big-ul (map (partial * d) ul)
        fold-start (mapv - l-start big-ul)
        fold-end (mapv + l-start big-ul)]
    [fold-start fold-end]))

(defn draw-level-01!
  "Called each frame, draws the current scene to the screen"
  [{:keys [current-scene] :as state}]
  (c/draw-background! coral-pink)
  (let [{:keys [draw-line? l-start l-end paper]} (get-in state [:scenes current-scene])
        l-end (or l-end (i/mouse-pos state))]
    ;; draw the paper
    (shape/fill-poly! state [0 0] paper p/white)
    ;; draw the points
    (doseq [p paper]
      (let [size 10
            offset [(/ size 2) (/ size 2)]]
        (shape/draw-ellipse! state (mapv - p offset) [size size] p/black)))
    (when draw-line?
      ;; draw actual line
      (shape/draw-line! state l-start l-end p/cyan :line-width 4)
      ;; calculate fold line
      (let [[fold-start fold-end :as fold-line] (project-fold-line state l-start l-end)]
        ;; draw fold line
        (when (seq (last fold-line))
          (shape/draw-line! state fold-start l-start p/red)
          (shape/draw-line! state l-end fold-end p/red)

          ;; tmp show reflections in the fold-line
          (shape/fill-ellipse! state (i/mouse-pos state) [10 10] p/magenta)
          (shape/fill-ellipse! state (reflect (i/mouse-pos state)
                                              fold-line)
                               [10 10] p/yellow)

          )
        ;; highlight paper edge intersections
        (doseq [[a b :as line] (u/poly-lines paper)]
          (when (collision/lines-intersect? line fold-line)
            (shape/draw-line! state a b p/green :line-width 3)
            ;; show intersection points
            (let [p (line-intersection-point line fold-line)
                  size 10
                  offset [(/ size 2) (/ size 2)]]
              (shape/draw-ellipse! state (mapv - p offset) [size size] p/black))))))))

(defn update-level-01
  "Called each frame, update the sprites in the current scene"
  [state]
  (-> state
      sprite/update-state))

(defn fold-paper
  [{:keys [current-scene] :as state}]
  (let [{:keys [l-start l-end paper]} (get-in state [:scenes current-scene])
        [f-start f-end :as fold-line] (project-fold-line state l-start l-end)
        ;; @TODO: tween the change, for now snapping to it is fine
        {:keys [any-intersected? points]}
        (reduce (fn [{:keys [points] :as acc} [a b :as line]]
                  (if (collision/lines-intersect? fold-line line)
                    (let [p (line-intersection-point line fold-line)]
                      (-> acc
                          (assoc :points (into points [p b]))
                          (assoc :any-intersected? true)))
                    (assoc acc :points (into points [b]))))
                {:any-intersected? false
                 :points [(first paper)]}
                (u/poly-lines paper))

        new-paper-groups (partition-by (partial u/left-turn? f-start f-end) (butlast points))

        new-paper (if any-intersected?
                    (mapcat (fn [ps]
                              (if (u/left-turn? f-start f-end (first ps))
                                ps
                                (map #(reflect % fold-line) ps)))
                            new-paper-groups)
                    (apply concat new-paper-groups))]

    ;; @TODO: reflecting breaks triangulation! we need to add a new shape, not just move points. take a look at folding an ear of the paper.

    (assoc-in state [:scenes current-scene :paper] (vec new-paper))))

(defn handle-click
  [{:keys [current-scene] :as state} e]
  (if (i/is e :button i/M_1)
    (let [{:keys [draw-line? l-start l-end]} (get-in state [:scenes current-scene])]
      (cond
        (and draw-line? l-end)
        (-> state
            (assoc-in [:scenes current-scene :draw-line?] false)
            (assoc-in [:scenes current-scene :l-start] nil)
            (assoc-in [:scenes current-scene :l-end] nil))

        (and draw-line?
             l-start
             (not= l-start (:pos e)))
        (-> state
            (assoc-in [:scenes current-scene :l-end] (:pos e))
            fold-paper)

        :else
        (-> state
            (assoc-in [:scenes current-scene :draw-line?] true)
            (assoc-in [:scenes current-scene :l-start] (:pos e)))))
    state))

(defn init
  "Initialise this scene"
  [state]
  {:sprites (sprites state)
   :draw-fn draw-level-01!
   :update-fn update-level-01
   :mouse-button-fns [handle-click]
   :draw-line? false
   :l-start nil
   :l-end nil
   :paper [[200 250]
           [200 350]
           [600 350]
           [600 250]]})
