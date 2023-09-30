(ns robot-world.dsl2
  (:require [robot-world.helpers :as helpers]))


(defn- clear-block [world block]
  (if-let [[stack-index index-of-block-above stack] (helpers/validate-clear-block block world)]
    (let [blocks-to-return (subvec stack index-of-block-above)
          updated-world (assoc world stack-index (subvec stack 0 index-of-block-above))]
      (helpers/return-blocks blocks-to-return updated-world))
    world))

(defn- move-pile [world [src-block src-stack-index] dest-stack-index]
   (let [src-stack (nth world src-stack-index)
         block-index-in-stack (helpers/index-of src-block src-stack)
         blocks-pile (subvec src-stack block-index-in-stack)
         updated-world (assoc world
                             src-stack-index (subvec src-stack 0 block-index-in-stack)
                             dest-stack-index (into (nth world dest-stack-index) blocks-pile))]
     updated-world))

(defn- move-to [world src-block dest-block]
   (let [src-stack-index (helpers/find-stack src-block world)
         dest-stack-index (helpers/find-stack dest-block world)]
   (move-pile world [src-block src-stack-index] dest-stack-index)))

(defn validate-command [world src-block dest-block]
   (let [src-stack-index (helpers/find-stack src-block world)
         dest-stack-index (helpers/find-stack dest-block world)]
     (cond
       (or (nil? src-stack-index) (nil? dest-stack-index) (= src-stack-index dest-stack-index))
      (throw (ex-info "Invalid command" {:src-stack-index src-stack-index
                                          :dest-stack-index dest-stack-index
                                          :world world}))
      :else world)))



(defmacro map-args-in-steps [args-map & steps]
  `(map (fn [step#]
          (map (fn [arg#]
                 (if (keyword? arg#)
                   (get ~args-map arg#)
                   arg#))
               step#))
        [~@steps]))

(defmacro command [name args & steps]
     `(defn ~name [world# & args#]
          (try
            (let [steps# (map-args-in-steps (into {} (map vector ~args args#)) ~@steps)
                  program# `(-> ~world#
                                (validate-command ~@args#)
                                ~@steps#)]
              (eval program#))
            (catch Exception e#
              (println "Ignoring invalid command" ~name args# (ex-data e#))
              world#))))

(command move-onto [:src-block :dest-block]
  [clear-block :src-block]
  [clear-block :dest-block]
  [move-to :src-block :dest-block])

(command move-over [:src-block :dest-block]
  [clear-block :src-block]
  [move-to :src-block :dest-block])

(command pile-onto [:src-block :dest-block]
  [clear-block :dest-block]
  [move-to :src-block :dest-block])

(command pile-over [:src-block :dest-block]
  [move-to :src-block :dest-block])
