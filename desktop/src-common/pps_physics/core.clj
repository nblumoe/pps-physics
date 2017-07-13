(ns pps-physics.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer :all]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :refer :all]
            [play-clj.repl :as repl]
            [play-clj.ui :refer :all]))

(def params (atom {:circle-density 1e20
                   :circle-friction 0.1
                   :circle-restitution 0.5
                   :rect-density 1e9
                   :rect-friction 0.99
                   :rect-restitution 0.0
                   :world-gravity -80
                   :ball-initial-x nil}))

(defn create-circle-body!
  [screen radius type]
  (let [body (add-body! screen (if (= type :static)
                                 (body-def :static)
                                 (body-def :dynamic)))]
    (->> (circle-shape :set-radius radius
                       :set-position (vector-2 radius radius))
         (fixture-def :density (:circle-density @params)
                      :friction (:circle-friction @params)
                      :restitution (:circle-restitution @params)
                      :shape)
         (body! body :create-fixture))
    (body! body :set-fixed-rotation true)
    body))

(defn create-circle-entity!
  [screen radius]
  (let [circle (texture "circle.png")]
    (assoc circle
           :body (create-circle-body! screen radius :static)
           :width (* 2 radius) :height (* 2 radius))))

(defn create-rect-body!
  [screen width height type]
  (let [body (add-body! screen (if (= type :static)
                                 (body-def :static)
                                 (body-def :dynamic)))]
    (->> (polygon-shape :set-as-box (/ width 2) (/ height 2))
         (fixture-def :density (:rect-density @params)
                      :friction (:rect-friction @params)
                      :restitution (:rect-restitution @params)
                      :shape)
         (body! body :create-fixture))
    body))

(defn create-floor-entity!
  [screen width height]
  (let [floor (texture "floor.png")]
    (assoc floor
           :body (create-rect-body! screen width height :static)
           :width width :height (/ height 2))))

(defn create-bin-entity!
  [screen radius]
  (let [floor (texture "floor.png")]
    (assoc floor
           :body (create-circle-body! screen radius :static)
           :width (* 2 radius) :height (* 2 radius))))

(defn create-static-world-entities!
  [screen]
  (let [circle-1 (doto (create-circle-entity! screen 1.5)
                   (body-position! 1.2 5 0))
        circle-2 (doto (create-circle-entity! screen 1.2)
                   (body-position! 6 3 0))
        floor-blocks (map #(let [entity (create-floor-entity! screen 3 1)]
                             (body-position! entity (* % 3) 0 0)
                             entity)
                          (range 4))
        floor-bin-spacing 0.5
        floor-bin-width 0.05
        floor-bins (map #(let [entity (create-bin-entity! screen floor-bin-width)]
                           (body-position! entity (* % floor-bin-spacing) 0.5 0)
                           entity)
                        (range 20))
        walls [(doto (create-floor-entity! screen 0.2 20)
                 (body-position! 0 0.5 0))
               (doto (create-floor-entity! screen 0.2 20)
                 (body-position! 9.8 0.5 0))]]
    (concat [circle-1 circle-2] floor-blocks floor-bins walls)))

(defn create-ball-entity!
  [screen]
  (let [ball (texture "ball.png")
        radius 0.3]
    (assoc ball
           :body (create-circle-body! screen radius :dynamic)
           :width (* 2 radius) :height (* 2 radius))))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [gravity (:world-gravity @params)
          screen (update! screen
                          :renderer (stage)
                          :camera (orthographic :set-to-ortho false 10 10)
                          :world (box-2d 0 gravity))
          num-balls 1]
      (concat (create-static-world-entities! screen)
              (repeatedly num-balls #(doto (assoc (create-ball-entity! screen) :ball? true)
                                       (body-position! (:ball-initial-x @params) 10 0))))))

  :on-render
  (fn [screen entities]
    (clear!)
    (->> entities
         (step! screen)
         (render! screen))))

(defgame pps-physics-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)))

(defn get-ball
  []
  (first (filter :ball? (repl/e main-screen))))

(defn ball-rests?
  [old-ball new-ball]
  (if (and old-ball
           (> 0.01 (Math/abs (- (:x old-ball) (:x new-ball))))
           (> 0.01 (Math/abs (- (:y old-ball) (:y new-ball)))))
    new-ball
    (do
      (Thread/sleep 150)
      (ball-rests? new-ball (get-ball)))))

(defn simulate
  [new-params]
  (reset! params new-params)
  (on-gl (set-screen! pps-physics-game main-screen))
  (ball-rests? nil (get-ball)))

(defn run-simulations
  ([n]
   (run-simulations n #(assoc @params :ball-initial-x (+ 0.3 (rand 9)))))
  ([n create-params]
   (-> (repeatedly n #(->> (:x (simulate (create-params)))
                           (format "%.1f")))
       frequencies
       time)))

(defscreen blank-screen
  :on-render
  (fn [screen entities]
    (clear!)))

(set-screen-wrapper! (fn [screen screen-fn]
                       (try (screen-fn)
                            (catch Exception e
                              (.printStackTrace e)
                              (set-screen! pps-physics-game blank-screen)))))
