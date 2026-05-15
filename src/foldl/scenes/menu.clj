(ns foldl.scenes.menu
  (:require [clunk.core :as c]
            [clunk.input :as input]
            [clunk.palette :as p]
            [clunk.scene :as scene]
            [clunk.sprite :as sprite]
            [clunk.util :as u]
            [foldl.sprites.button :as button]))

(def green (p/hex->rgba "#60D394"))

(defn on-click-play
  "Transition from this scene to `:level-01` with a ~500ms frame
  fade-out"
  [state e]
  (scene/transition state :level-01 :transition-length 40))

(defn sprites
  "The initial list of sprites for this scene"
  [{:keys [window] :as state}]
  [(-> (button/button-sprite (u/center window) "Play")
       (input/add-on-click on-click-play))])

(defn draw-menu!
  "Called each frame, draws the current scene to the screen"
  [state]
  (c/draw-background! green)
  (sprite/draw-scene-sprites! state))

(defn update-menu
  "Called each frame, update the sprites in the current scene"
  [state]
  (-> state
      sprite/update-state))

(defn init
  "Initialise this scene"
  [state]
  {:sprites (sprites state)
   :draw-fn draw-menu!
   :update-fn update-menu})
