(ns foldl.scenes.level-01
  (:require [clunk.collision :as collision]
            [clunk.core :as c]
            [clunk.input :as i]
            [clunk.palette :as p]
            [clunk.shape :as shape]
            [clunk.sprite :as sprite]
            [clunk.util :as u]
            [foldl.sprites.origami :as origami]))

(def coral-pink (p/hex->rgba "#FF9B85"))

(defn rand-colour
  []
  (p/rand-colour)
  #_(rand-nth [p/red p/green p/blue
             p/yellow p/magenta p/cyan
             p/white p/grey p/black]))

(defn initial-paper
  []
  (origami/origami
   [(origami/paper
     [[200 250]
      [200 350]
      [600 350]
      [600 250]]
     :colour p/white)]))

(defn sprites
  "The initial list of sprites for this scene"
  [{:keys [window] :as state}]
  [(initial-paper)])

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
      (let [m (if (zero? dx)
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
  (sprite/draw-scene-sprites! state)
  (let [{:keys [draw-line? l-start l-end] :as scene} (get-in state [:scenes current-scene])
        l-end (or l-end (i/mouse-pos state))
        origamis (filter (sprite/has-group :origami) (:sprites scene))]

    ;; @TODO: fold line should be it's own sprite probably, don't love the global state in the scene, but maybe it's fine
    
    (when draw-line?
      ;; draw actual line
      (shape/draw-line! state l-start l-end p/cyan :line-width 4)
      ;; calculate fold line
      (let [[fold-start fold-end :as fold-line] (project-fold-line state l-start l-end)]
        ;; draw fold line
        (when (seq (last fold-line))
          (shape/draw-line! state fold-start l-start p/red)
          (shape/draw-line! state l-end fold-end p/red))))))

(defn update-level-01
  "Called each frame, update the sprites in the current scene"
  [state]
  (-> state
      sprite/update-state))

(defn fold-paper
  [{:keys [points colour layer] :as paper} [f-start f-end :as fold-line]]
  (let [;; @TODO: tween the change, for now snapping to it is fine

        ;; add new points into poly at intersections
        {:keys [points i-points]}
        (reduce (fn [{:keys [points i-points] :as acc} [a b :as line]]
                  (if (collision/lines-intersect? fold-line line)
                    (let [p (line-intersection-point line fold-line)]
                      (-> acc
                          (assoc :points (into points [p b]))
                          (assoc :i-points (into i-points [p]))))
                    (assoc acc :points (into points [b]))))
                {:points [(first points)]
                 :i-points []}
                (u/poly-lines points))]
    ;; do nothing if no intersections
    (if (empty? i-points)
      {:new-shapes [paper]
       :i-points []}
      ;; split into new shapes
      (let [i-pairs (u/poly-lines i-points) ; using poly-lines, [a b c] => [[a b] [b c] [c a]]
            point-loop (take 100 (cycle points))
            new-shapes (map (fn [[i1 i2]]
                              (let [begin (drop-while #(not= i1 %) point-loop)
                                    shape (take-while #(not= i2 %) begin)
                                    extra (first (drop (count shape) begin))]
                                {:points (vec (concat shape [extra]))
                                 :colour (rand-colour)
                                 :layer layer}))
                            i-pairs)]
        {:new-shapes new-shapes
         :i-points i-points}))))

(defn fold-origami
  [{:keys [papers] :as origami} [f-start f-end :as fold-line]]
  (let [top-layer (apply max (map :layer papers))
        fold-results (map #(fold-paper % fold-line) papers)
        new-papers (mapcat :new-shapes fold-results)
        i-points (into #{} (mapcat :i-points fold-results))]
    (if (seq i-points)
      ;; group-by the predicate so we can reverse the layers of the folded papers, then add top-layer to each
      (let [{flipping true static false}
            (group-by (fn [paper]
                        (every? #(u/left-turn? f-start f-end %)
                                (remove i-points
                                        (:points paper))))
                      new-papers)
            flipping-layer-indices (sort (map :layer flipping))
            reflected (map (fn [paper layer]
                             (if (some #(u/left-turn? f-start f-end %) (remove i-points (:points paper)))
                               (-> paper
                                   (assoc :points (vec (reverse (map #(reflect % fold-line) (:points paper)))))
                                   (assoc :layer layer))
                               paper))
                           (sort-by :layer flipping)
                           (reverse flipping-layer-indices))
            layer-offset (map #(update % :layer + (inc top-layer)) reflected)]
        (assoc origami :papers (vec (concat static layer-offset))))
      (assoc origami :papers new-papers))))

(defn fold-all-papers
  [{:keys [current-scene] :as state}]
  (let [{:keys [l-start l-end sprites]} (get-in state [:scenes current-scene])
        fold-line (project-fold-line state l-start l-end)]
    (sprite/update-sprites
     state
     (sprite/has-group :origami)
     #(fold-origami % fold-line))))

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
            fold-all-papers)

        :else
        (-> state
            (assoc-in [:scenes current-scene :draw-line?] true)
            (assoc-in [:scenes current-scene :l-start] (:pos e)))))
    state))

(defn handle-key
  [{:keys [current-scene] :as state} e]
  (if (i/is e :key i/K_ESCAPE)
    ;; reinitialise sprites
    (assoc-in state [:scenes current-scene :sprites] (sprites state))
    state))

(defn init
  "Initialise this scene"
  [state]
  {:sprites (sprites state)
   :draw-fn draw-level-01!
   :update-fn update-level-01
   :mouse-button-fns [handle-click]
   :key-fns [handle-key]
   :draw-line? false
   :l-start nil
   :l-end nil})
