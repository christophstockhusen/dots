(ns dots.core
  (:gen-class)
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def colors
  [[207 240 158]
   [162 219 168]
   [121 189 154]
   [59 134 134]
   [11 72 107]])

(def sizes-and-counts
  [{:size 2000 :count 10}
   {:size 1000 :count 20}
   {:size 500 :count 100}
   {:size 100 :count 1000}])

(defn circles-collide?
  [circle1 circle2]
  (let [[x1 y1 d1 _] circle1
        [x2 y2 d2 _] circle2]
    (> (+ (/ d1 2) (/ d2 2))
       (Math/sqrt (+ (Math/pow (- x1 x2) 2)
                     (Math/pow (- y1 y2) 2))))))

(defn random-circle
  [size]
  [(rand-int 10000) (rand-int 10000) size (rand-nth colors)])

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
  (q/ellipse x y d d))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (q/background 240)
  (q/color-mode :rgb)
  (q/no-stroke)
  (compute-circles sizes-and-counts))

(defn draw-state [state]
  (let [circles state]
    (doall (map draw-circle circles)))
    (q/save "test.png"))

(defn -main
  []
  (q/sketch
   :title "dots"
   :size [10000 10000]
   :setup setup
   :draw draw-state
   :features [:keep-on-top :exit-on-close]
   :middleware [m/fun-mode]))