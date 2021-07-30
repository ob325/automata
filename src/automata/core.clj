(ns automata.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def board-size 50) ;; size of 1d array
(def square-size 10)
(def rule (atom -1))

(def possible-neighborhoods
  (let [states (range 2)]
    (reverse (for [left states
                   center states
                   right states]
               [left center right]))))

(defn dec->bin
  "Returns n length string of x's binary representation"
  [x]
  (let [s (Long/toString x 2)
        len (count s)
        pad (apply str (repeat (- 8 len) "0"))]
    (as-> (str pad s) s
      (seq (char-array s))
      (map #(Character/digit % 10) s))))

(defn step
  "Returns the result of applying rule to x"
  [rule x]
  (let [cells (map vec (partition 3 1 x))
        rule-map (apply hash-map (interleave possible-neighborhoods (dec->bin rule)))]
    (as-> (mapv rule-map cells) new
      (conj new 0)
      (vec (concat [0] new)))))

(defn setup []
  (q/rect-mode :corner)
  (q/text-size  30)
  (q/frame-rate 6)
  (q/color-mode :hsb)
  [])

(defn update-state [state]
  (swap! rule inc)
  (reduce (fn [acc v]
            (conj acc (step @rule (last acc))))
          [(vec (flatten (concat [(repeat (/ board-size 2) 0)] [1]
                                 [(repeat (/ board-size 2) 0)])))]
          (range 50)))

(defn draw-state [state]
  (q/background 255)
  (doseq [x (range board-size)
          y (range board-size)]
    (q/fill 0)
    (q/text (str @rule) (+ 50 (* board-size square-size)) 50)
    (q/text (apply str (dec->bin @rule)) (+ 50 (* board-size square-size)) 150)
    (q/stroke 255)
    (q/fill (if (zero? (get-in state [y x])) 255 0))
    (q/rect (* x square-size) (* y square-size)
            square-size square-size)))


(q/defsketch automata
  :title "Cellular automata"
  :size [800 512]
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])
