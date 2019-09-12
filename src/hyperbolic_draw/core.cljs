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

(defn poincareInfinityPointsFromGeodesic [center radius]
  (let [xc  (first center)
        yc (second center)]
    (let [dSqrd  (+ (q/sq xc)  (q/sq yc))]
      (let [d (q/sqrt dSqrd)
            inter (+ (- dSqrd (q/sq radius)) 1.0)]
        (let [x (/ inter (* 2 d))
              y (q/sqrt (- 1.0 (q/sq x)))
              angle (q/acos (/ xc d))]
          (let [xr1 (- (* x (q/cos angle)) (* y (q/sin angle)))
                yr1 (+ (* x (q/sin angle)) (* y (q/cos angle)))
                xr2 (+ (* x (q/cos angle)) (* y (q/sin angle)))
                yr2 (- (* x (q/sin angle)) (* y (q/cos angle)))]
            ;(println [xr1 yr1 xr2 yr2])
            (if (< yc 0) [{:pos [xr1 (- yr1)]} {:pos [xr2 (- yr2)]}] [{:pos [xr1 yr1]} {:pos [xr2 yr2]}])))))))

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

(defn addPt [pt1 pt2]
  {:pos [(+ (first pt1) (first (:pos pt2))) (+ (second pt1) (second (:pos pt2)))]}
  )
(defn math-add [a b]
  (.add js/math a b))

(defn math-multiply [a b]
  (.multiply js/math a b))

(defn math-divide [a b]
  (.divide js/math a b))

(defn moebius [a b c d pt]
  (let [cpt (.complex js/math (first (:pos pt)) (second (:pos pt)))]
    (let [w (math-divide (math-add (math-multiply a cpt) b) (math-add (math-multiply c cpt) d))]
      {:pos [(aget w "re") (aget w "im")]}
      )
  )
)


(defn mouse-dragged [state event]
  (let [ end-pt (from-screenspace-to-normalspace [(:x event) (:y event)] )
         start-pt (from-screenspace-to-normalspace [(:p-x event) (:p-y event)] )]
    (let [ dx ( - (first end-pt) (first start-pt))
          dy ( - (second end-pt) (second start-pt))]
      ;(println (moebius 2 1 -1 2 {:pos end-pt}))
      ;(println (:points state))
      (assoc state :points (map (partial moebius 1 0.2 0.2 1) (:points state)))
      ;(assoc state :points (map (partial addPt [dx dy]) (:points state) ))
      )))

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
        (let [bpts (poincareInfinityPointsFromGeodesic center radius)] 
          (q/ellipse (first (from-normalspace-to-screenspace  (:pos (first bpts)))) (second (from-normalspace-to-screenspace  (:pos (first bpts)))) point-size point-size)
          (q/ellipse (first (from-normalspace-to-screenspace  (:pos (second bpts)))) (second (from-normalspace-to-screenspace (:pos (second bpts)))) point-size point-size))
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
     :mouse-dragged mouse-dragged
     :draw draw-state
     :middleware [m/fun-mode]))
