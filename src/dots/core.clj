(ns dots.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def colors
  [[207 240 158]
   [162 219 168]
   [121 189 154]
   [59 134 134]
   [11 72 107]])

(defn add-circle
  [circle-list]
  (conj circle-list [(rand-int 1000) (rand-int 1000) 100 (rand-nth colors)]))

(defn compute-circles
  []
  (loop [n 100
         circles []]
    (if (zero? n)
      circles
      (recur (dec n) (add-circle circles)))))

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
  (compute-circles))

(defn draw-state [state]
  (let [circles state]
    (doall (map draw-circle circles))))

(q/defsketch hello-quil
  :title "You spin my circle right round"
  :size [1000 1000]
  :setup setup
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode])
