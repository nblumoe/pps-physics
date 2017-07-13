(ns pps-physics.core.desktop-launcher
  (:require [pps-physics.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. pps-physics-game "pps-physics" 800 600)
  (Keyboard/enableRepeatEvents true))
