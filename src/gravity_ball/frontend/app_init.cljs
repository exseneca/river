(ns gravity-ball.frontend.app-init
  (:require
   [reagent.core :as r]
   [reagent.dom]))

(enable-console-print!)
; TODO: frontend changes require a refresh
(defn cubed [x]
  (* x x x))
(defn squared [x]
  (* x x))

(defn coordinates [x y]
  [x y])
(defn get-x [coordinates]
  (first coordinates))
(defn get-y [coordinates]
  (second coordinates))

(defn vector-add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn vector-multiply [[x y] d]
  [(* d x) (* d y)])

(defn vec-magnitude [[x y]]
  (Math/sqrt (+ (squared x) (squared y))))
;(def hitting-banks-a? (r/atom false))

(def boat-radius 5)
(def waves (atom nil))
(def boat-state (atom {:location [300.0 300.0]
                       :velocity [0.0 0.0]
                       :acceleration [0.0 0.0]}))
(defn bezier-by-offset [y]
  (let [y-vec [0 y]]
    {:point-0 (vector-add [-17 129] y-vec)
     :point-1 (vector-add [0 500] y-vec)
     :point-2 (vector-add [400 0] y-vec)
     :point-3 (vector-add [800 136] y-vec)}))

;;(def bezier-river-bottom {:point-0 [-17 329] :point-1 [0 700] :point-2 [400 200] :point-3 [800 336]})
(def bezier-river-bottom (bezier-by-offset 200))
;;(def bezier-river-top {:point-0 [-17 129] :point-1 [0 500] :point-2 [400 0] :point-3 [800 136]})
(def bezier-river-top (bezier-by-offset 0))
(defn create-bezier-on-line [base-line y-offset]
  (into {}
        (map (fn [[k [vx vy]]] [k [vx (+ y-offset vy)]]) base-line)))

(defn flow-beziers []
  (vec (map (partial create-bezier-on-line bezier-river-top)
            (range 20 200 30))))

;; TODO: document the maths
(defn bezier-term [t p0 p1 p2 p3]
  (let [one-minus-t (- 1 t)]
    (+ (* (cubed one-minus-t) p0)
       (* 3 (squared one-minus-t) t p1)
       (* 3 one-minus-t (squared t) p2)
       (* (cubed t) p3))))

(defn bezier-gradient-term [t p0 p1 p2 p3]
  (+ (* (- 3) (- 1 t) (- t) p0)
     (* 3
        (+ 1 (* t -4) (* 3 (squared t)))
        p1)
     (* 3
        (- (* 2 t) (* 3 (squared t)))
        p2)
     (* 3 (squared t) p3)))

(defn intersection-x-term [t
                           [x-0 _]
                           [x-1 _]
                           [x-2 _]
                           [x-3 _]
                           [x-c _]
                           ]
  (let [one-minus-t (- 1 t)]
    (+ (* (cubed one-minus-t)
          x-0)
       (* 3 (squared one-minus-t) t x-1)
       (* 3 one-minus-t (squared t) x-2)
       (* (cubed t) x-3)
       (- x-c))))

(defn intersection-y-term [t
                           [_ y-0]
                           [_ y-1]
                           [_ y-2]
                           [_ y-3]
                           [_ y-c]
                           ]
  (let [one-minus-t (- 1 t)]
    (+ (* (cubed one-minus-t)
                     y-0)
       (* 3 (squared one-minus-t) t y-1)
       (* 3 one-minus-t
          (squared t) y-2)
       (* (cubed t) y-3) (- y-c))))

(defn intersection-fun [
                        [x-0 y-0]
                        [x-1 y-1]
                        [x-2 y-2]
                        [x-3 y-3]
                        [x-c y-c]
                        r
                        t]
  (let [x-term (intersection-x-term
                t
                [x-0 y-0]
                [x-1 y-1]
                [x-2 y-2]
                [x-3 y-3]
                [x-c y-c]
                )
        y-term (intersection-y-term
                t
                [x-0 y-0]
                [x-1 y-1]
                [x-2 y-2]
                [x-3 y-3]
                [x-c y-c]
                )
        minus-r-squared (- (squared r))]
    (+ (squared x-term) (squared y-term) minus-r-squared)))



(defn div-term [t a0 a1 a2 a3]
  (+ (* (- 3) (- 1 t) (- t) a0)
     (* 3
        (+ 1
           (* t -4)
           (* 3 (squared t)))
        a1)
     (* 3
        (- (* 2 t)
           (* 3 (squared t)))
        a2)
     (* 3 (squared t) a3)))

(defn intersection-fun-derivative [
                        [x-0 y-0]
                        [x-1 y-1]
                        [x-2 y-2]
                        [x-3 y-3]
                        [x-c y-c]
                                   r
                                   t]
  (+ (* 2 (intersection-x-term t
                        [x-0 y-0]
                        [x-1 y-1]
                        [x-2 y-2]
                        [x-3 y-3]
                        [x-c y-c]
                        )
        (div-term t x-0 x-1 x-2 x-3))
     (* 2 (intersection-y-term t
                        [x-0 y-0]
                        [x-1 y-1]
                        [x-2 y-2]
                        [x-3 y-3]
                        [x-c y-c]
                        )
        (div-term t y-0 y-1 y-2 y-3))))


(defn intersection-fun-bezier [bezier]
  (let [{:keys [point-0 point-1 point-2 point-3]} bezier]
    (partial intersection-fun point-0 point-1 point-2 point-3)))

(defn intersection-deriv-bezier [bezier]
  (let [{:keys [point-0 point-1 point-2 point-3]} bezier]
    (partial intersection-fun-derivative point-0 point-1 point-2 point-3)))

(defn intersection-fun-bezier-circle [bezier circle]
  (let [{:keys [centre radius]} circle]
    (partial (intersection-fun-bezier bezier) centre radius)))

(defn intersection-deriv-bezier-circle [bezier circle]
  (let [{:keys [centre radius]} circle]
    (partial (intersection-deriv-bezier bezier) centre radius)))

(defn next-newton [f ft t]
  (- t
   (/ (f t)
      (ft t))))

(defn solve-newton [f f-dash max-tries tolerance]
  (let [get-next-newton-guess (partial next-newton f f-dash)]
    (loop [tries 0
           guess 0.5] ;; Should initial guess be a paramter? 
      (let [next-guess (get-next-newton-guess guess)]
        (cond (> tolerance (abs (- guess next-guess)))
              next-guess
              (> tries max-tries)
              nil
              :else
              (recur (inc tries) next-guess))))))


(defn intersecting? [bezier circle]
  (let [f (intersection-fun-bezier-circle bezier circle)
        ft (intersection-deriv-bezier-circle bezier circle)]
    (solve-newton f ft 100 0.00001)))

(defn add-x-boat-velocity [boat dx]
  (let [[x y] (:velocity @boat)
        new-velocity [(+ x dx) y]]
    (when (> 3 (vec-magnitude new-velocity))
      (reset! boat (assoc @boat :velocity new-velocity)))))

(defn hitting-banks? [top bottom new-location boat-radius]
  (or (intersecting? top
                     {:centre new-location
                      :radius boat-radius})
      (intersecting? bottom
                     {:centre new-location
                      :radius boat-radius})))

(defn add-x-boat-location [boat dx]
  (let [[x y] (:location @boat)
        new-location [(+ x dx) y]
        hitting-banks? (or (intersecting? bezier-river-top
                                          {:centre new-location
                                           :radius boat-radius})
                           (intersecting? bezier-river-bottom
                                          {:centre new-location
                                           :radius boat-radius}))]
;    (println "Hitting banks? " hitting-banks?)
 ;   (reset! hitting-banks-a? hitting-banks?)
    (when (not hitting-banks?)
      (reset! boat (assoc @boat :location new-location)))))

(defn add-y-boat-location [boat dy]
  (let [[x y] (:location @boat)
        new-location [x (+ dy y)]
        hitting-banks? (or (intersecting? bezier-river-top
                                          {:centre new-location
                                           :radius boat-radius})
                           (intersecting? bezier-river-bottom
                                          {:centre new-location
                                           :radius boat-radius}))]

  ;  (reset! hitting-banks-a? hitting-banks?)
    (when (not hitting-banks?)
      (reset! boat (assoc @boat  :location new-location)))))

(defn add-y-boat-velocity [boat dy]
  (let [[x y] (:velocity @boat)
        new-velocity [x (+ dy y)]]
    (when (> 3 (vec-magnitude new-velocity))
     (reset! boat (assoc @boat :velocity new-velocity)))))

(defn bezier-position [bezier t]
  (let [{:keys [point-0 point-1 point-2 point-3]} bezier
        [x0 y0] point-0
        [x1 y1] point-1
        [x2 y2] point-2
        [x3 y3] point-3]
    [(bezier-term t x0 x1 x2 x3)
     (bezier-term t y0 y1 y2 y3)]))

(defn bezier-gradient [bezier t]
  (let [{:keys [point-0 point-1 point-2 point-3]} bezier
        [x0 y0] point-0
        [x1 y1] point-1
        [x2 y2] point-2
        [x3 y3] point-3]
    ;; Because beziers are paremetric we must do
    ;; (dy/dt)/(dx/dt) to get (dy/dx)
    (/ (bezier-gradient-term t y0 y1 y2 y3)
       (bezier-gradient-term t x0 x1 x2 x3))))

(defn unit-tangent [point gradient]
  (let [[x1 y1] point
        y-intercept (- y1 (* gradient x1))
        x2 (+ 5 x1)
        y2 (+ y-intercept (* gradient x2))
        tangent-vector (vector-add [x2 y2] (vector-multiply point -1))
        tangent-length (Math/sqrt (+ (squared x2) (squared y2)))]
    (vector-multiply tangent-vector (/ 1 tangent-length))))

(defn bezier-function-for-flow [bezier x t]
  (let [{:keys [point-0 point-1 point-2 point-3]} bezier
        [x0 _] point-0
        [x1 _] point-1
        [x2 _] point-2
        [x3 _] point-3]
    ;; Solving bezier = x for t. So the function is 
    (- (bezier-term t x0 x1 x2 x3)
       x)))

(defn bezier-gradient-for-flow [bezier t]
  (let [{:keys [point-0 point-1 point-2 point-3]} bezier
        [x0 _] point-0
        [x1 _] point-1
        [x2 _] point-2
        [x3 _] point-3]
    (bezier-gradient-term t x0 x1 x2 x3)))

(defn bezier-t-at-point [bezier-flow [xp _]]
  (let [f (partial bezier-function-for-flow bezier-flow xp)
        ft (partial bezier-gradient-for-flow bezier-flow)]
    (solve-newton f ft 100 0.00001)))

(defn flow-at-point [bezier-flow flow-multiplier point]
  (let [t (bezier-t-at-point bezier-flow point)
        gradient (bezier-gradient bezier-flow t)]
    (vector-multiply (unit-tangent point gradient) flow-multiplier)))

;; create a vector from the point going right.
;; y = mx + c => c = (y - m.x) (use the current point to get that.
;; Now get a new point. P2 by taking x + 1, y2 = (x + 1)m + c ;
;; Now we have two points P1 = (x, y), P2 = (x + 1, y2)
;; We want a vector take (xt, yt) = P2 - P1 to be vector of the tangent
;; now divide by lenght sqrt(xt^2 + yt^2)
;; unit tangent = (xt, yt) / sqrt(xt^2 + yt^2);

(defn make-wave-point [bezier t]
  (let [bezier-position-fn (partial bezier-position bezier)
        bezier-gradient-fn (partial bezier-gradient bezier)
        gradient (bezier-gradient-fn t)
        point (bezier-position-fn t)]
    {:point point
     :gradient gradient
     :unit-tangent (unit-tangent point gradient)
     :location-fn bezier-position-fn
     :gradient-fn bezier-gradient-fn
     :t t}))
;; TODO: waves as lines

(defn initial-wave-location [bezier]
  (let [ts (range 0 1 0.020)
        offset (/ (rand) 100)
        moved (mapv (partial + offset) ts)]
    (mapv (partial make-wave-point bezier) moved)))

(defn get-initial-wave-points [beziers]
  (flatten (mapv initial-wave-location beziers)))

(def event-queue (atom []))

(defn add-to-event-queue [q evt]
  (swap! q (fn [x] (conj x evt)))) 

(defn take-from-event-queue
  [q]
  (let [evts @q]
    (reset! event-queue [])
    evts))

(defn app []
  [:canvas#c {:height 800 :width 800}])

(defn get-canvas []
  (.getElementById js/document "c"))

(defn get-canvas-ctx []
  (.getContext (get-canvas) "2d"))

(defn draw-circle [ctx radius coordinates]
  (let [x (get-x coordinates)
        y (get-y coordinates)]
    (.beginPath ctx)
    (.arc ctx x y radius 0 (* 2 Math/PI))
    (.fill ctx)))

(defn clear-canvas [ctx]
  (.clearRect ctx 0 0 800 800))

(defn draw-bezier [ctx bezier]
  (let [{:keys [point-0 point-1 point-2 point-3]} bezier
        [x0 y0] point-0
        [x1 y1] point-1
        [x2 y2] point-2
        [x3 y3] point-3]
    (.beginPath ctx)
    (.moveTo ctx x0 y0)
    (.bezierCurveTo ctx x1 y1 x2 y2 x3 y3)
    (.stroke ctx)))

(defn draw-river [ctx]
   (.beginPath ctx)
  (.moveTo ctx 0 0)
  ;; WARNING: Any changes to to this must be reflected in the top bezier.
  (.lineTo ctx -17 129)
 ;(.bezierCurveTo ctx 0 400 600 0 696 136)
  (.bezierCurveTo ctx 0 500 400 0 800 136)
  (.lineTo ctx 800 0)
  
  ;; TODO: parameterize river
  ;
  ;(.moveTo ctx -17 129)
  ;(.lineTo ctx -17 329)

  (.closePath ctx)
  (.fill ctx)

  (.beginPath ctx)
  (.moveTo ctx 0 800)
  (.lineTo ctx -17 329)
  (.bezierCurveTo ctx 0 700 400 200 800 336)
  (.lineTo ctx 800 800)
  (.closePath ctx)
  (.fill ctx))

(defn draw-line [ctx p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    (.beginPath ctx)
    (.moveTo ctx x1 y1)
    (.lineTo ctx x2 y2)
    (.stroke ctx)))

(def wave-type :line)

(def printed (atom 0))
(defn special-print [& more]
  (when (< @printed 100)
    (apply println more)
    (swap! printed inc)))

(defn draw-wave [ctx wave-type wave]
  (case wave-type
    :point
    (draw-circle ctx 1 (:point wave))
    :line
    (let [end-point (vector-add (:point wave) (vector-multiply (:unit-tangent wave) 1000.0))]
      (draw-line ctx (:point wave) end-point))))

(defn draw []
  (let [ctx (get-canvas-ctx)]
    (clear-canvas ctx)
    (draw-river ctx)
    (doseq [wave @waves]
      (draw-wave ctx wave-type wave))
    (draw-circle ctx boat-radius (:location @boat-state))))

(defn render
  []
  (reagent.dom/render
   [app]
   (.getElementById js/document "root")))
(defn get-events [] (take-from-event-queue event-queue))

(def d 0.3)

(defn process-events [evts]
  (doseq [evt evts]
    (cond (= evt :left)
          (add-x-boat-velocity boat-state (- d))
          (= evt :right)
          (add-x-boat-velocity boat-state d)
          (= evt :up)
          (add-y-boat-velocity boat-state (- d))
          (= evt :down)
          (add-y-boat-velocity boat-state d))))

(def wave-increment 0.0005)
(defn update-wave [wave-increment
                   {:keys [location-fn gradient-fn point t]}]
  (let [try-t (+ wave-increment t)
        new-t (if (> try-t 1) 0.0 try-t)
        point (location-fn new-t)
        gradient (gradient-fn new-t)]
    {:point point
     :unit-tangent (unit-tangent point gradient)
     :location-fn location-fn
     :gradient-fn gradient-fn
     :gradient gradient
     :t new-t}))

(defn update-boat [cfg boat]
  (let [{:keys [flow-multiplier bezier-flow]} cfg
        bt @boat
        old-location (:location bt)
        old-velocity (:velocity bt)
        old-acceleration (:acceleration bt)
        new-location (vector-add old-location old-velocity)
        [xl yl] new-location
        new-acceleration (flow-at-point bezier-flow flow-multiplier new-location)
        new-velocity (vector-add old-velocity  old-acceleration)
        mag-velocity (vec-magnitude new-velocity)
        clipped-velocity (if (> mag-velocity 0.57) old-velocity new-velocity)
        new-boat (if (hitting-banks? bezier-river-top bezier-river-bottom new-location boat-radius)
                   {:location old-location
                    :velocity [0 0] ;; We could rebound? maybe see how that goes? 
                    :acceleration [0 0]}
                   {:location new-location
                    :velocity clipped-velocity
                    :acceleration new-acceleration})]
    (reset! boat new-boat)))

(defn update-waves [wave-increment waves]
  (let [new-waves (mapv (partial update-wave wave-increment) @waves)]
    (reset! waves new-waves)))

(defn update-state []
  (update-boat {:flow-multiplier 0.5
                :bezier-flow bezier-river-top}
               boat-state)
  (update-waves wave-increment waves))

(defn game-loop []
  (let [evts (get-events)]
    (process-events evts)
    (update-state)
    (draw)
    (.requestAnimationFrame js/window game-loop)))


(defn add-event-to-event-queue [kw]
  (add-to-event-queue event-queue kw))
(defn add-up-to-event-queue []
  (add-event-to-event-queue :up))
(defn add-down-to-event-queue []
  (add-event-to-event-queue :down))
(defn add-left-to-event-queue []
  (add-event-to-event-queue :left))
(defn add-right-to-event-queue []
  (add-event-to-event-queue :right))

(defn fire-keyboard-evt [evt]
  (case (.-key evt)
    "ArrowRight" (add-right-to-event-queue)
    "ArrowDown" (add-down-to-event-queue)
    "ArrowLeft" (add-left-to-event-queue)
    "ArrowUp" (add-up-to-event-queue)
    nil))

(defn add-click-listener []
  (let [canv (get-canvas)]
    (.addEventListener js/document "keydown" (fn [evt] (.preventDefault evt) 
                                               (fire-keyboard-evt evt)))))
;; TODO: arrows can make the boat go faster than it should
;; TODO: Collision pushes boat up.
;; TODO: boat that rotates in the right direction.
;; TODO: Should we change the dynamics? Just left and right rotate space rows. How would that work?
;; TODO: Something weird is going on with the unit tangent.. most weird closer to zero.
;; TODO: The boat should never be faster than the flow.
;; TODO: Bug that allows the boat to get stuck. This is probably tunneling.
;; There's probably some floating point nastiness going on.

(defn ^:export init 
  []
  (render)
  (add-click-listener)
  (reset! waves (get-initial-wave-points (flow-beziers)))
  (game-loop))
