(ns baby.pat.td
  (:require [baby.pat.jes.vt :as vt]
            [clojure.string]
            [exoscale.coax :as c]
            [orchestra.core #?(:clj :refer :cljs :refer-macros) [defn-spec]]
            [tick.core]))


(def now tick.core/now)
(defn-spec !instant! ::vt/instant []
  "Returns now as an instant."
  (tick.core/now))

(defn-spec timestamp->instant ::vt/instant
  "Takes the milliseconds off the time string of a `malformed` database time string and makes it an inst."
  [at ::vt/str]
  (let [t-added     (-> at (clojure.string/replace #" " "T"))
        dropped-sub (-> t-added (clojure.string/split #"\.") first)
        inst-string (str dropped-sub "Z")]
    (tick.core/instant inst-string)))

(defn-spec instant->map ::vt/map
  "Takes an instant of time and breaks it down into units."
  [t ::vt/instant]
  (baby.pat.vt/add-kw-ns :time
                         {:yyyy (tick.core/int (tick.core/year t))
                          :MM   (tick.core/int (tick.core/month t))
                          :dd   (tick.core/day-of-month t)
                          :HH   (tick.core/hour t)
                          :mm   (tick.core/minute t)
                          :ss   (tick.core/second t)
                          :ms   (long (/ (tick.core/nanosecond t) 1000000))
                          :ns   (tick.core/nanosecond t)}))

(c/def ::vt/instant->map (fn [x opts] (when (tick.core/instant? x) (instant->map x))))

(defn-spec map->instant ::vt/instant
  "Takes a map of time and returns an instant."
  [{:time/keys [yyyy MM dd HH mm ss ms]} ::vt/map]
  (let [frmt   (fn [VA] (if (= 1 (count (str VA))) (str "0" VA) VA))
        month  (frmt MM)
        day    (frmt dd)
        hour   (frmt HH)
        minute (frmt mm)
        second (frmt ss)]
    (tick.core/instant (str yyyy "-" month "-" day "T" hour ":" minute ":" second "." ms "Z"))))

(c/def ::vt/map->instant (fn [x opts] (when (map? x) (map->instant x))))

(defn-spec time-pair->seconds ::vt/long
  "Turns a pair of number/unit i.e. 12 :minutes and converts it to seconds."
  ([p ::vt/time-pair]
   (let [years 31536000
         weeks 604800
         days  86400
         hours 3600]
     (* (first p) (case (second p)
                    :yy      years
                    :yyyy    years
                    :years   years
                    :weeks   weeks
                    :dd      days
                    :days    days
                    :hh      hours
                    :HH      hours
                    :hours   hours
                    :mm      60
                    :minutes 60
                    :ss      1
                    :seconds 1))))
  ([n ::vt/long u ::vt/kw] (time-pair->seconds [n u]))
  ([n ::vt/long u ::vt/kw & nus ::vt/coll] (reduce + (map time-pair->seconds (partition 2 (flatten [n u nus]))))))

(defn-spec sum ::vt/instant-or-num
  "Adds time by taking an inst, and number/unit pairs. i.e. 12 :minutes 43 :hours 4 :days, etc. Adds numbers via reduce."
  [original ::vt/instant-or-num & others ::vt/coll]
  (if (tick.core/instant? original)
    (let [s (reduce + (for [p (partition 2 others)]
                        (time-pair->seconds p)))]
      (tick.core/instant (tick.core/>> original
                                       (tick.core/new-duration s :seconds))))
    (reduce + original others)))

(defn-spec subtract ::vt/instant-or-num
  "Subtracts time by taking an inst, and number/unit pairs. i.e. 12 :minutes 43 :hours 4 :days, etc. Subtracts numbers via reduce."
  [original ::vt/instant-or-num & others ::vt/coll]
  (if (tick.core/instant? original)
    (let [s (reduce + (for [p (partition 2 others)]
                        (time-pair->seconds p)))]
      (tick.core/instant (tick.core/<< original (tick.core/new-duration s :seconds))))
    (reduce - original others)))

(defn-spec before? ::vt/?
  "Is the first thing before the second?"
  [established ::vt/instant-or-num in-question ::vt/instant-or-num]
  (if (tick.core/instant? established)
    (tick.core/< established in-question)
    (< established in-question)))

(defn-spec after? ::vt/?
  "Is the first thing after the second?"
  [established ::vt/instant-or-num in-question ::vt/instant-or-num]
  (if (tick.core/instant? established)
    (tick.core/> established in-question)
    (> established in-question)))

(defn-spec at-or-before? ::vt/?
  "Is the first thing at or before the second?"
  [established ::vt/instant-or-num in-question ::vt/instant-or-num]
  (if (tick.core/instant? established)
    (tick.core/<= established in-question)
    (<= established in-question)))

(defn-spec at-or-after? ::vt/?
  "Is the first thing at or after the second?"
  [established ::vt/instant-or-num in-question ::vt/instant-or-num]
  (if (tick.core/instant? established)
    (tick.core/>= established in-question)
    (>= established in-question)))

(defn-spec not-before? ::vt/?
  "Is the first thing not before the second?"
  [established ::vt/instant-or-num in-question ::vt/instant-or-num]
  (not (before? established in-question)))

(defn-spec not-after? ::vt/?
  "Is the first thing not after the second?"
  [established ::vt/instant-or-num in-question ::vt/instant-or-num]
  (not (after? established in-question)))

(defn-spec between? ::vt/?
  "Given the start and end is the thing in question inside both?"
  [basis-start ::vt/instant-or-num basis-end ::vt/instant-or-num in-question ::vt/instant-or-num]
  (and (after? basis-start in-question)
       (before? basis-end in-question)))

(defn-spec in-bounds? ::vt/?
  "Given the start and end is the thing in question within both?"
  [basis-start ::vt/instant-or-num basis-end ::vt/instant-or-num in-question ::vt/instant-or-num]
  (and (at-or-before? basis-end in-question)
       (at-or-after? basis-start in-question)))

(defn-spec duration-in-seconds ::vt/long
  "Gives the duration of a time range in seconds. Useful for qualifying drilling activities based on sparse info."
  [time-start ::vt/instant time-end ::vt/instant]
  (-> (tick.core/duration {:tick/beginning time-start
                           :tick/end       time-end})
      tick.core/seconds))
