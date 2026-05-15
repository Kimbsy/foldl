(ns foldl.core
  (:gen-class)
  (:require [clunk.core :as c]
            [foldl.scenes.level-01 :as level-01]
            [foldl.scenes.menu :as menu]))

(defn init-scenes
  "Map of scenes in the game"
  [state]
  {:menu (menu/init state)
   :level-01 (level-01/init state)})

;; Configure the game
(def foldl-game
  (c/game {:title "foldl"
           :size [800 600]
           :init-scenes-fn init-scenes
           :current-scene :level-01
           :assets {:image {:captain-spritesheet "resources/img/captain.png"}}}))

(defn -main
  "Run the game"
  [& args]
  (c/start! foldl-game))
