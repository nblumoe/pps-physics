(ns pps-physics.pp
  (:require [anglican.core :refer :all]
            [anglican.emit :refer :all]
            anglican.importance
            [anglican.inference :refer :all]
            anglican.lmh
            [anglican.runtime :refer :all]
            [anglican.state :refer :all]
            [clojure.edn :as edn]
            incanter.charts
            incanter.core
            incanter.io
            incanter.stats
            [pps-physics.core :as physics]))

(with-primitive-procedures [pps-physics.core/simulate]
  (defquery physics
    [observations]
    (let [target-pos 5.5
          params {:circle-density 1e20
                  :circle-friction 0.1
                  :circle-restitution (sample (uniform-continuous 0 0.8))
                  :rect-density 1e9
                  :rect-friction 0.99
                  :rect-restitution 0.0
                  :world-gravity -80 #_(* -1 (sample (gamma 70 1)))
                  :ball-initial-x (sample (uniform-continuous 0.3 9.3))}
          ball-final (simulate params)]
      #_(map #(observe (normal ball-final 1.5) %) observations)
      (observe (normal ball-final 1.5) target-pos)
      params)))

(defn read-data
  [file]
  (edn/read-string (slurp (str (name file) ".edn"))))

(defn persist-data
  [file data]
  (let [filename (str (name file) ".edn")
        old-data (read-data file)
        new-data (vec (concat old-data data))]
    (spit filename new-data)))

(def gravity-prior (time (doall (repeatedly 1e5 #(* -1 (sample* (gamma 70 1)))))))

(defn java-color
  [[r g b a]]
  (java.awt.Color. r g b a))

(def default-colors
  (let [opacity         220
        radical-red     [255 53 94 opacity]
        amaranth        [229 43 80 opacity]
        amaranth-purple [171 39 79 opacity]
        leaf-green      [68 147 99 opacity]
        blue            [0 0 200 opacity]]
    (map java-color [amaranth leaf-green blue amaranth-purple radical-red])))

(defn plot-results
  []
  (let [theme (doto (org.jfree.chart.StandardChartTheme/createJFreeTheme)
                (.setXYBarPainter (org.jfree.chart.renderer.xy.StandardXYBarPainter.))
                (.setShadowVisible false)
                (.setDrawingSupplier
                 (org.jfree.chart.plot.DefaultDrawingSupplier.
                  (into-array java.awt.Paint default-colors)
                  org.jfree.chart.plot.DefaultDrawingSupplier/DEFAULT_FILL_PAINT_SEQUENCE
                  org.jfree.chart.plot.DefaultDrawingSupplier/DEFAULT_OUTLINE_PAINT_SEQUENCE
                  org.jfree.chart.plot.DefaultDrawingSupplier/DEFAULT_STROKE_SEQUENCE
                  org.jfree.chart.plot.DefaultDrawingSupplier/DEFAULT_OUTLINE_STROKE_SEQUENCE
                  org.jfree.chart.plot.DefaultDrawingSupplier/DEFAULT_SHAPE_SEQUENCE)))]
    #_(-> (incanter.charts/histogram
           gravity-prior
           :legend false
           :x-label "g"
           :nbins 30
           :theme theme
           :density true)
          (incanter.charts/add-histogram
           (map :world-gravity (map :result (read-data :results)))
           :nbins 30
           :theme theme
           :density true)
          (incanter.charts/set-y-range 0 0.1)
          (incanter.charts/set-x-range -100 -35)
          (incanter.core/save "results_gravity.png"
                              :width 1024 :height 1024)
          #_incanter.core/view)
    (-> (incanter.charts/histogram
         (map :circle-restitution (map :result (read-data :results)))
         :legend false
         :x-label "restitution"
         :nbins 20
         :theme theme
         :density true)
        (incanter.charts/set-x-range 0 1)
        (incanter.core/save "results_circle-restitution.png"
                            :width 1024 :height 1024)
        #_incanter.core/view)
    (-> (incanter.charts/histogram
         (map :ball-initial-x (map :result (read-data :results)))
         :legend false
         :x-label "ball initial x"
         :nbins 20
         :theme theme
         :density true)
        (incanter.core/save "results_ball-initial-x.png"
                            :width 1024 :height 1024)
        #_incanter.core/view)))

(comment

  (def observations (time (doall (physics/run-simulations 200))))

  (persist-data :observations observations)

  (time
   (let [results (doquery :lmh physics [(read-data :observations)])]
     (loop [batch 0
            rs    results]
       (when (< batch 1e4)
         (persist-data :results [(first rs)])
         (recur (inc batch) (next rs))))))


  (-> (incanter.charts/histogram
       (map :world-gravity (map :result (read-data :results)))
       :legend false
       :x-label "g"
       :nbins 30
       :density true)
      (incanter.charts/add-histogram
       gravity-prior
       :nbins 30
       :density true)
      incanter.core/view)

  )
