(ns robot-world.dsl
(:require [robot-world.helpers :as helpers]))

(defn- clear-block [block-world block]
  (if-let [[stack-index index-of-block-above stack] (helpers/validate-clear-block block block-world)]
    (let [blocks-to-return (subvec stack index-of-block-above)
          updated-world (assoc block-world stack-index (subvec stack 0 index-of-block-above))]
      (helpers/return-blocks blocks-to-return updated-world))
    block-world))

(defn- clear-src-block [world src-block & _]
  (clear-block src-block world))

(defn- clear-dest-block [world _ dest-block]
  (clear-block dest-block world))

(defn- move-pile [[src-block src-stack-index] dest-stack-index block-world]
  (let [src-stack (nth block-world src-stack-index)
        block-index-in-stack (helpers/index-of src-block src-stack)
        blocks-pile (subvec src-stack block-index-in-stack)
        updated-world (assoc block-world
                             src-stack-index (subvec src-stack 0 block-index-in-stack)
                             dest-stack-index (into (nth block-world dest-stack-index) blocks-pile))]
    updated-world))

(defn- move-to [world src-block dest-block]
  (let [src-stack-index (helpers/find-stack src-block world)
        dest-stack-index (helpers/find-stack dest-block world)]
  (move-pile [src-block src-stack-index] dest-stack-index world)))

(defn- reset-world [world & _]
  (mapv vector (range 1 (inc (count world)))))

(defmacro command [name & steps]
  `(def ~name
     (fn [world# & args#]
       (let [steps# (list ~@steps)]
         (reduce (fn [world# step#]
                   (apply step# world# args#))
                 world#
                 steps#)))))

(command move-onto
  clear-src-block
  clear-dest-block
  move-to)

(command move-over
  clear-src-block
  move-to)

(command pile-onto
  clear-dest-block
  move-to)

(command pile-over
  move-to)

(command clear
  clear-src-block)

(command reset
  reset-world)

(comment
  (defn apply-mapped-args [world step]
    (apply (first step) (vec (conj (rest step) world))))

  (defn map-args-in-steps [args-map steps]
    (map (fn [step]
           (map (fn [arg]
                  (if (keyword? arg)
                    (get args-map arg)
                    arg))
                step))
         (vec steps)))

  (defmacro map-args-in-steps [args-map & steps]
    `(map
      (fn [step#]
        (map
         (fn [arg#]
           (if (keyword? arg#)
             (get ~args-map arg#)
             arg#))
         step#))
      [~@steps]))

  (map
   (fn [step]
     (map
      (fn [arg]
        (if (keyword? arg)
          (get {:src-block 2, :dest-block 3} arg)
          arg))
      step))
   [[clear-block :src-block]])

  (defmacro command2 [name args & steps]
    `(def ~name
       (fn [world# & args#]
           ;; (println "world" world# "args" args#)
         (let [steps# (map-args-in-steps (into {} (map vector ~args args#)) ~@steps)] steps#))))
                ;; (println "steps" steps#)
            ;;     ]
            ;; (reduce (fn [world# step#]
            ;;           (println "applying" (vec (conj (rest step#) world#)))
            ;;           (apply (first step#) (vec (conj (rest step#) world#))))
            ;;         world#
                    ;; steps#
         ;; (let [args-map# (into {} (map vector ~args args#))
         ;;       steps# ~@(map-args-in-steps
         ;;                       'args-map#
         ;;                       'steps)])))
           ;; (reduce (fn [world# step#]
           ;;           (println "applying" (vec (conj (rest step#) world#)))
           ;;           world#)
           ;;           ;; (apply (first step#) (vec (conj (rest step#) world#))))
           ;;         world#
           ;;         steps#)))))
           ;; 'steps#)))
  ;; )

  (into {} (map vector [:a :b] [1 2]))
  ;; (defmacro command3 [name args & steps]
  ;;   `(def ~name
  ;;      (fn [world# ~args]
  ;;        (let [args-map# (into {} (map vector '~args [~@args]))]
  ;;          (reduce (fn [world# step#]
  ;;                    (let [evaluated-step#
  ;;                          (map-indexed
  ;;                           (fn [idx# arg#]
  ;;                             (if (keyword? arg#)
  ;;                               (get args-map# arg#)
  ;;                               arg#))
  ;;                           step#)]
  ;;                      (apply (first evaluated-step#) world# (rest evaluated-step#))))
  ;;                  world#
  ;;                  (list ~@steps))))))

  (println ((command2 move-onto2 [:src-block :dest-block]
  [clear-block :src-block]
  [clear-block :dest-block]
  [move-to :src-block :dest-block]) [[1] [2] [3] [4]] 2 3))

  ,)
(comment
  (println (let [args-map {:src-block 2 :dest-block 3}]
    (map
     (fn [step]
       (map
        (fn [arg]
          (if (keyword? arg)
            (get args-map arg)
            arg))
        step))
     [[clear-block :src-block] [clear-block :dest-block] [move-to :src-block :dest-block]])))
  (assoc [clear-block :src-block] 1 (get {:src-block 2 :dest-block 3} :src-block))

  (map-indexed
   (fn [i arg]
     (if (keyword? arg)
       (assoc step i (get args-map arg))
       arg)))

  (let [step '(clear-block 2)]
    (println "(apply"(first step) (vec (conj (rest step) [[] [2 1 3 4] [] []])) ")")
    (apply (first step) (vec (conj (rest step) [[] [2 1 3 4] [] []]))))

  (apply clear-src-block [[[] [2 1 3 4] [] []] 2] )

   ,)
