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
  ;; @TODO: We've switched to memoized triangulation in clunk, but we
  ;; could also write a fill-polys! which allows us to specify
  ;; multiple colours.
  (doseq [{:keys [points colour] :as paper} (sort-by :layer papers)]
    ;; draw the paper
    (shape/fill-poly! state [0 0] points colour)))

(defn origami
  [papers]
  (sprite/sprite
   :origami
   [0 0]
   :draw-fn draw-origami!
   :extra {:papers papers}))
