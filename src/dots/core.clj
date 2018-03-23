(ns dots.core
  (:gen-class)
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn scale
  [x]
  (* x 10000))

;; 
(def colors
  [[236 208 120]
   [217 91 67]
   [192 41 66]
   [84 36 55]
   [83 119 122]])

;; http://www.color-hex.com/color-palette/660
(def colors
  [[255 203 133]
   [253 245 201]
   [194 242 208]
   [255 197 217]
   [107 62 38]])

;; http://www.color-hex.com/color-palette/1294
(def colors
  [[1 31 75]
   [3 57 108]
   [0 91 150]
   [100 151 177]
   [179 205 224]])

(def sizes-and-counts
  [{:size 0.2 :count 2}
   {:size 0.1 :count 10}
   {:size 0.05 :count 100}
   {:size 0.03 :count 1000}
   {:size 0.01 :count 50000}
   {:size 0.005 :count 500000}])

(defn circles-collide?
  [circle1 circle2]
  (let [[x1 y1 d1 _] circle1
        [x2 y2 d2 _] circle2]
    (> (+ (/ d1 2) (/ d2 2) 0.005)
       (Math/sqrt (+ (Math/pow (- x1 x2) 2)
                     (Math/pow (- y1 y2) 2))))))

(defn gaussian
  [shift factor]
  (max (min (+ shift (* factor (q/random-gaussian))) 1) 0))

(defn random-circle
  [size]
  (let [r (/ size 2)
        margin 0.01
        x (+ r margin (rand (- 1 (* r 2) (* margin 2))))
        y (+ r margin (rand (- 1 (* r 2) (* margin 2))))
        c1 (nth colors (int (* (gaussian y 0.25) (dec (count colors)))))
        c2 (rand-nth colors)]
    (if (> 0.03 size)
      [x y size c1]
      [x y size c2])))

(defn add-circle
  [circle-list circle]
  (if (every? #(not (circles-collide? circle %)) circle-list)
    (conj circle-list circle)
    circle-list))

(defn add-circles
  [circles final-circles]
  (reduce add-circle final-circles circles))

(defn random-circle-list
  [n size]
  (map random-circle (repeat n size)))

(defn compute-circles
  [sizes-and-counts]
  (loop [sizes-and-counts sizes-and-counts
         circles []]
    (if (empty? sizes-and-counts)
      circles
      (recur (rest sizes-and-counts)
             (add-circles (random-circle-list (:count (first sizes-and-counts)) (:size (first sizes-and-counts))) circles)))))

(defn draw-circle
  [[x y d [r g b]]]
  (q/fill r g b)
  (q/ellipse (scale x) (scale y) (scale d) (scale d)))

(defn setup []
  (q/frame-rate 30)
  (q/background 255 255 240)
  (q/color-mode :rgb)
  (q/no-stroke)
  (compute-circles sizes-and-counts))

(defn draw-state [state]
  (let [circles state]
    (doall (map draw-circle circles)))
  (q/save "result.png"))

(defn -main
  []
  (q/sketch
   :title "dots"
   :size [10000 10000]
   :setup setup
   :draw draw-state
   :features [:keep-on-top :exit-on-close]
   :middleware [m/fun-mode]))
