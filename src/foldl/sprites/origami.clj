(ns foldl.sprites.origami
  "An Origami is a collection of pieces of paper which fold together."
  (:require [clunk.palette :as p]
            [clunk.shape :as shape]
            [clunk.sprite :as sprite]))

(defn paper
  [points &
   {:keys [colour layer]
    :or {colour (p/rand-colour)
         layer 0}}]
  {:points points
   :colour colour
   :layer layer})

(defn draw-origami!
  [state {:keys [papers]}]
  (doseq [{:keys [points colour] :as paper} (sort-by :layer papers)]
    ;; draw the paper
    (shape/fill-poly! state [0 0] points colour)
    ;; draw the points
    (doseq [p points]
      (let [size 10
            offset [(/ size 2) (/ size 2)]]
        (shape/draw-ellipse! state (mapv - p offset) [size size] p/black)))))

(defn origami
  [papers]
  (sprite/sprite
   :origami
   [0 0]
   :draw-fn draw-origami!
   :extra {:papers papers}))
