(ns hyperbolic-draw.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def point-size 6)

(defn setup []
  (q/frame-rate 25)
  (q/background 255)
  (q/ellipse-mode :radius)
  {:points []})

(defn from-screenspace-to-normalspace [pt]
    [(- (/ (first pt) (/ (q/width) 2)) 1.0)
        (- (/ (second pt) (/ (q/height) 2)) 1.0)]
  )
(defn from-normalspace-to-screenspace [pt]
    [ (* (+(first pt)  1.0) (/ (q/width) 2))
      (* (+(second pt) 1.0) (/ (q/height) 2)) ]
  )

(defn get-lamp-index []
  (let [pt (from-screenspace-to-normalspace [(q/mouse-x) (q/mouse-y)]) ]
  (if (< (+ (q/sq (first pt)) (q/sq (second pt))) 1)
    pt
    nil)))

;;(defn update-state [state]
;;  (if-let [index (get-lamp-index)]
;;    (assoc state index (create-color (q/millis)))
;;    state))

(defn poincareArcCenterFromLine [pt1 pt2]
  (let [u0 (first pt1)
        u1 (second pt1)
        v0 (first pt2)
        v1 (second pt2)]
    (let [divisor (* (- (* u0 v1) (* u1 v0)) -2)
          umag (+ (q/sq u0) (q/sq u1))
          vmag (+ (q/sq v0) (q/sq v1))]
      [(/ (- (* u1 (+ vmag 1)) (* v1 (+ umag 1))) divisor)
       (/ (- (* v0 (+ umag 1)) (* u0 (+ vmag 1))) divisor)]
    )))

(defn poincareArcAngles [pt1 pt2 center]
  (let [ angles (sort [(q/atan2 (- (second pt1) (second center)) (- (first pt1) (first center)))
   (q/atan2 (- (second pt2) (second center)) (- (first pt2) (first center)))]) ]
    (if (> (q/abs (- (first angles) (second angles)) ) q/PI)
      (sort [  (+ (first angles) q/TWO-PI) (second angles)])
      angles
  )))

(defn poincareArcRadiusFromCenter [center]
        (q/sqrt (q/abs (- (- 1  (q/sq (first center))) (q/sq (second center)))))
)

(defn mouse-clicked [state event]
  (if-let [index (get-lamp-index)]
    (assoc state :points (conj (:points state) {:pos index }))
    state))

(defn draw-geodesic [pt1 pt2]
  (let [spt1  (from-normalspace-to-screenspace pt1)
        spt2 (from-normalspace-to-screenspace pt2)]
  (q/fill 153 0 0)
  (q/ellipse (first spt1) (second spt1) point-size point-size)
  (q/ellipse (first spt2) (second spt2) point-size point-size)
  (q/stroke 255 255 255)
  ;;(q/line (first spt1) (second spt1) (first spt2) (second spt2))
  (q/no-fill)
  (let [center (poincareArcCenterFromLine pt1 pt2)]
      (let [scenter (from-normalspace-to-screenspace center)
            radius (poincareArcRadiusFromCenter center)
            angles (poincareArcAngles pt1 pt2 center)]
        (q/arc (first scenter) (second scenter) (* radius (/ (q/width) 2)) (* radius (/ (q/width) 2)) (first angles) (second angles)
          )))))

(defn draw-state [state]
  (q/no-stroke)
  (q/background 255)
  (q/no-stroke)
  (q/fill 0 0 0)
  (let [w (q/width)
        h (q/height)
        hw (/ w 2)
        hh (/ h 2)]
      (q/ellipse hw hh hw hh)
    (doseq [[pt1 pt2] (partition 2 1 (:points state))]
        (draw-geodesic (:pos pt1) (:pos pt2)))))


(defn ^:export run-sketch []
  (q/defsketch hyperbolic-draw
     :host "hyperbolic-draw"
     :size [500 500]
     :setup setup
     :mouse-clicked mouse-clicked
     :draw draw-state
     :middleware [m/fun-mode]))
