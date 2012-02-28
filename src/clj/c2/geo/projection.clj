(ns c2.geo.projection
  "Map projections taking [longitude, latitude] to [x, y]."
  (:use [c2.maths :only [radians-per-degree
                         sin cos sqrt]]))


(defrecord Albers [origin parallels scale translate]
  clojure.lang.IFn
  (applyTo [this args] (map this args))
  (invoke [this [x y]]
    (let [phi1 (* radians-per-degree (first parallels))
          phi2 (* radians-per-degree (second parallels))
          lng0 (* radians-per-degree (first origin))
          lat0 (* radians-per-degree (second origin))

          s (sin phi1), c (cos phi1)
          n (* 0.5 (+ s (sin phi2)))
          C (+ (* c c) (* 2 n s))
          p0 (/ (sqrt (- C (* 2 n (sin lat0)))) n)

          t (* n (- (* radians-per-degree x)
                    lng0))
          p (/ (sqrt (- C (* 2 n (sin (* radians-per-degree y)))))
               n)
          ]
      [(+ (* scale p (sin t)) (first translate))
       (+ (* scale (- (* p (cos t)) p0))
          (second translate))])))

(defn albers
  "The Albers equal-area conic projection.
   See http://mathworld.wolfram.com/AlbersEqual-AreaConicProjection.html"
  [& {:keys [origin parallels scale translate]
      :or {origin [-98 38]
           parallels [29.5, 45.5]
           scale 1000
           translate [480 250]}}]
  (Albers. origin parallels scale translate))

(defn albers-usa
  "Albers projection with Alaska, Hawaii, and Puerto Rico scaled/translated to fit nicely with each other"
  [& args]
  (let [lower48 (apply albers args)
        alaska  (assoc lower48
                  :origin [-160 60]
                  :prallels [55 65])
        hawaii (assoc lower48
                 :origin [-160 20]
                 :prallels [8 18])
        puerto-rico (assoc lower48
                      :origin [-60 10]
                      :prallels [8 18])]

))
