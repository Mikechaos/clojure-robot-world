(ns robot-world.dsl2
  (:require [robot-world.helpers :as helpers]))


(defn- clear-block
  "Clears the specified block, returning any blocks above it to their original positions."
   [world block]
  (if-let [[stack-index index-of-block-above stack] (helpers/validate-clear-block block world)]
    (let [blocks-to-return (subvec stack index-of-block-above)
          updated-world (assoc world stack-index (subvec stack 0 index-of-block-above))]
      (helpers/return-blocks blocks-to-return updated-world))
    world))

(defn- move-pile
  "Moves the specified block (and any blocks above it) to the top of another stack."
  [world [src-block src-stack-index] dest-stack-index]
   (let [src-stack (nth world src-stack-index)
         block-index-in-stack (helpers/index-of src-block src-stack)
         blocks-pile (subvec src-stack block-index-in-stack)
         updated-world (assoc world
                             src-stack-index (subvec src-stack 0 block-index-in-stack)
                             dest-stack-index (into (nth world dest-stack-index) blocks-pile))]
     updated-world))

(defn- move-to
  "Moves the specified block (and any blocks above it) to the top of the stack containing `dest-block`."
  [world src-block dest-block]
   (let [src-stack-index (helpers/find-stack src-block world)
         dest-stack-index (helpers/find-stack dest-block world)]
   (move-pile world [src-block src-stack-index] dest-stack-index)))

(defn validate-command
  "Validates the command arguments and throws an exception if they are invalid.
   Invalid commands are those that try to move a blocks not in the block world,
   onto itself or onto a block that is the same stack."
  [world src-block dest-block]
   (let [src-stack-index (helpers/find-stack src-block world)
         dest-stack-index (helpers/find-stack dest-block world)]
     (cond
       (or (nil? src-stack-index) (nil? dest-stack-index) (= src-stack-index dest-stack-index))
      (throw (ex-info "Invalid command" {:src-stack-index src-stack-index
                                          :dest-stack-index dest-stack-index
                                          :world world}))
      :else world)))



;; Maps the symbolic arguments in each step to their actual values.
;;
;; Parameters:
;;   args-map - A map containing the symbolic argument names and their corresponding values.
;;              E.g.: {:src-block 1 :dest-block 2}
;;   steps - A sequence of steps, each containing functions and symbolic arguments.
;;           E.g.: [[clear-block :src-block] [clear-block :dest-block] [move-to :src-block :dest-block]]
;;
;; Returns:
;;   A sequence of steps with symbolic arguments replaced by their actual values.
;;   E.g.: [[clear-block 1] [clear-block 2] [move-to 1 2]]
(defmacro map-args-in-steps [args-map & steps]
  "Transforms the symbolic arguments in steps to their actual values."
  `(map (fn [step#]
          (map (fn [arg#]
                 (if (keyword? arg#)
                   (get ~args-map arg#)
                   arg#))
               step#))
        [~@steps]))

;; Defines a new command for the block world DSL.
;;
;; Parameters:
;;   name - The name of the command to define.
;;   args - A vector of symbolic argument names for the command.
;;          E.g.: [:src-block :dest-block] -> Defines a command that takes two arguments.
;;          Arguments can then be refered to in the steps using the same names
;;          (:keywords in clojure terms)
;;   steps - A sequence of steps, each containing functions and symbolic arguments.
;;
;; Defines:
;;   A new function with the given name that performs the specified steps in the block world.
(defmacro command [name args & steps]
  "Defines a new command for the block world DSL using the provided steps and arguments."
     `(defn ~name [world# & args#]
          (try
            ;; Replace symbolic arguments in steps with their actual values.
            (let [steps# (map-args-in-steps (into {} (map vector ~args args#)) ~@steps)
                  ;; Create a program that chains the steps using the thread-first macro.
                  program# `(-> ~world#
                                (validate-command ~@args#)
                                ~@steps#)]
              ;; Evaluate and execute the program.
              (eval program#))
            (catch Exception e#
              (println "Ignoring invalid command" ~name args# (ex-data e#))
              world#))))

;; Example commands using the macro:

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
