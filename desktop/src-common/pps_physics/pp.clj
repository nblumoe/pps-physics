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
                  :world-gravity (* -1 (sample (gamma 70 1)))
                  :ball-initial-x (sample (uniform-continuous 0.3 9.3))}
          ball-final (simulate params)]
      (map #(observe (normal ball-final 1.5) %) observations)
      params)))

;; model params and observe instruction for hitting the target-pos
#_-80
#_(observe (normal ball-final 1.5) target-pos)

(def gravity-prior (time (doall (repeatedly 1e5 #(* -1 (sample* (gamma 70 1)))))))

;; PERSISTENCE

(defn read-data
  [experiment file]
  (edn/read-string (slurp (str "results" "/" experiment "/" (name file) ".edn"))))

(defn persist-data
  [file data]
  (let [filename (str (name file) ".edn")
        old-data (read-data file)
        new-data (vec (concat old-data data))]
    (spit filename new-data)))

;; PLOTTING

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

(defn add-pointer
  [chart x y text]
  (.addAnnotation (.getPlot chart)
                  (doto (org.jfree.chart.annotations.XYTextAnnotation. text x y)
                    (.setFont (java.awt.Font. "SansSerif" java.awt.Font/PLAIN 28))))
  chart)

(defn add-true-value
  [chart x]
  (.addAnnotation (.getPlot chart)
                  (org.jfree.chart.annotations.XYLineAnnotation.
                   x 0 x 100
                   (java.awt.BasicStroke. 3)
                   java.awt.Color/BLACK))
  chart)

(defn add-margins
  [chart margin]
  (let [plot (.getPlot chart)]
    (doseq [axis [(.getRangeAxis plot) (.getDomainAxis plot)]]
      (.setUpperMargin axis margin)
      (.setLowerMargin axis margin)))
  chart)

(defn add-padding
  [chart padding]
  (.setPadding chart (org.jfree.ui.RectangleInsets. padding padding padding padding))
  chart)

(defn plot-results
  [experiment]
  (let [out-file #(str "results/" experiment  "/" % ".png")
        theme (doto (org.jfree.chart.StandardChartTheme/createJFreeTheme)
                (.setXYBarPainter (org.jfree.chart.renderer.xy.StandardXYBarPainter.))
                (.setShadowVisible false)
                (.setSmallFont (java.awt.Font. "SansSerif" java.awt.Font/PLAIN 24))
                (.setRegularFont (java.awt.Font. "SansSerif" java.awt.Font/PLAIN 28))
                (.setLargeFont (java.awt.Font. "SansSerif" java.awt.Font/PLAIN 42))
                (.setExtraLargeFont (java.awt.Font. "SansSerif" java.awt.Font/PLAIN 48))
                (.setDrawingSupplier
                 (org.jfree.chart.plot.DefaultDrawingSupplier.
                  (into-array java.awt.Paint default-colors)
                  org.jfree.chart.plot.DefaultDrawingSupplier/DEFAULT_FILL_PAINT_SEQUENCE
                  org.jfree.chart.plot.DefaultDrawingSupplier/DEFAULT_OUTLINE_PAINT_SEQUENCE
                  org.jfree.chart.plot.DefaultDrawingSupplier/DEFAULT_STROKE_SEQUENCE
                  org.jfree.chart.plot.DefaultDrawingSupplier/DEFAULT_OUTLINE_STROKE_SEQUENCE
                  org.jfree.chart.plot.DefaultDrawingSupplier/DEFAULT_SHAPE_SEQUENCE)))]
    (-> (incanter.charts/histogram
         gravity-prior
         :legend false
         :y-label "Probability Density"
         :x-label "Gravity"
         :nbins 20
         :theme theme
         :density true)
        (incanter.charts/add-histogram
         (map :world-gravity (map :result (read-data experiment :results)))
         :nbins 20
         :theme theme
         :density true)
        (incanter.charts/set-y-range 0 0.1)
        (incanter.charts/set-x-range -100 -35)
        (add-pointer -86 0.075 "True Value")
        (add-true-value -80)
        (add-margins 100)
        (add-padding 10)
        (incanter.core/save (out-file "gravity") :width 1024 :height 1024)
        #_incanter.core/view)
    (-> (incanter.charts/histogram
         (map :circle-restitution (map :result (read-data experiment :results)))
         :legend false
         :y-label "Probability Density"
         :x-label "Restitution"
         :nbins 20
         :theme theme
         :density true)
        (incanter.charts/set-x-range 0 1)
        (add-pointer 0.4 1.65 "True Value")
        (add-true-value 0.5)
        (add-padding 10)
        (incanter.core/save (out-file "restitution")
                            :width 1024 :height 1024)
        #_incanter.core/view)
    (-> (incanter.charts/histogram
         (map :ball-initial-x (map :result (read-data experiment :results)))
         :legend false
         :y-label "Probability Density"
         :x-label "Starting Point"
         :nbins 20
         :theme theme
         :density true)
        (add-padding 10)
        (incanter.core/save (out-file "starting_point")
                            :width 1024 :height 1024)
        #_incanter.core/view)))

;; RUN GRAVITY MODEL

(comment

  (def observations (time (doall (physics/run-simulations 200))))

  (persist-data :observations observations)

  (time
   (let [results (doquery :lmh physics [(read-data "gravity" :observations)])]
     (loop [batch 0
            rs    results]
       (when (< batch 1e2)
         (persist-data "gravity" :results [(first rs)])
         (recur (inc batch) (next rs))))))


  (-> (incanter.charts/histogram
       (map :world-gravity (map :result (read-data "gravity" :results)))
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
