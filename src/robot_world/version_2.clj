(ns robot-world.version-2)

(defn find-stack [block block-world]
  (first
   (keep-indexed (fn [idx stack]
                   (when (some #{block} stack) idx))
                 block-world)))

(defn replace-stack [idx new-stack block-world]
  (assoc block-world idx new-stack))

(defn return-blocks [blocks block-world]
  (reduce (fn [world block]
            (let [original-position (- block 1)] ; calculate the original stack of the block
              (update world original-position conj block))) ; return block to its original stack
          block-world
          blocks))

(defn index-of [item coll]
  (some (fn [[idx elem]] (when (= item elem) idx))
        (map-indexed vector coll)))

(defn validate-clear-block [block block-world]
  (when-let [stack-index (find-stack block block-world)]
    (when-let [idx (index-of block (nth block-world stack-index))]
      (if (> idx -1)
        [stack-index (inc idx) (nth block-world stack-index)]
        nil))))

(defn clear-block [block block-world]
  (if-let [[stack-index index-of-block-above stack] (validate-clear-block block block-world)]
    (let [blocks-to-return (subvec stack index-of-block-above)
          updated-world (assoc block-world stack-index (subvec stack 0 index-of-block-above))]
      (return-blocks blocks-to-return updated-world))
    block-world))

(comment
  (def world-now [[1 2 3 4] [] [] []])
  (def world-after (clear-block 2 world-now))
  world-after
  (println "world-after" world-after)
  ,)


(defn log-illegal-move-and-return [command-type src-block dest-block modifier-type ex-meta block-world]
  (println
   "Performing" command-type
   src-block modifier-type dest-block
   "- Invalid move"
   "in world" ex-meta)
  block-world)

(defn log-legal-command-and-return [command-type src-block dest-block modifier-type block-world new-world]
  (println
   "Performing" command-type
   src-block modifier-type dest-block
   " in " block-world " to get " new-world)
  new-world)

(defn move-block [[src-block src-stack-index] dest-stack-index block-world]
  (let [updated-world (update block-world src-stack-index (partial filterv (complement #{src-block})))
        updated-dest-stack (conj (nth updated-world dest-stack-index) src-block)]
    (assoc updated-world dest-stack-index updated-dest-stack)))

(defn move-pile [[src-block src-stack-index] dest-stack-index block-world]
  (let [src-stack (nth block-world src-stack-index)
        block-index-in-stack (index-of src-block src-stack)
        blocks-pile (subvec src-stack block-index-in-stack)
        updated-world (assoc block-world
                             src-stack-index (subvec src-stack 0 block-index-in-stack)
                             dest-stack-index (into (nth block-world dest-stack-index) blocks-pile))]
    updated-world))

(defn validate-world-before-command [src-block dest-block block-world]
  (let [src-stack-index (find-stack src-block block-world)
        dest-stack-index (find-stack dest-block block-world)]
    (cond
      (or (nil? src-stack-index) (nil? dest-stack-index) (= src-stack-index dest-stack-index))
      (throw (ex-info "Invalid command" {:src-stack-index src-stack-index
                                         :dest-stack-index dest-stack-index
                                         :block-world block-world}))
      :else [src-stack-index dest-stack-index])))

(defn perform-command [command-type src-block dest-block modifier-type block-world command-fn]
  (try
    (let [[src-stack-index dest-stack-index]
          (validate-world-before-command src-block dest-block block-world)
          new-world (cond->> block-world
                      (= command-type "move") (clear-block src-block)
                      (= modifier-type :onto) (clear-block dest-block)
                      true (command-fn [src-block src-stack-index] dest-stack-index))]
      (log-legal-command-and-return command-type src-block dest-block modifier-type block-world new-world))
    (catch Exception e
      (log-illegal-move-and-return
       command-type src-block dest-block modifier-type (ex-data e) block-world))))

(defn move [src-block dest-block move-type block-world]
  (perform-command "move" src-block dest-block move-type block-world move-block))

(defn pile [src-block dest-block pile-type block-world]
  (perform-command "pile" src-block dest-block pile-type block-world move-pile))

(defn initialize-world [n]
  (println "Initializing world" n (mapv vector (range 1 (inc n))))
  (mapv vector (range 1 (inc n))))

(defn clear-src-block [world src-block & _]
  (clear-block src-block world))

(defn clear-dest-block [world _ dest-block]
   (clear-block dest-block world))


(defn place-block-onto [world src-block dest-block]
  (let [src-stack-index (find-stack src-block world)
        dest-stack-index (find-stack dest-block world)]
    (move-block [src-block src-stack-index] dest-stack-index world)))


(defmacro command [name & body]
  `(def ~name (fn [world# & [~@body]]
                ~@(map (fn [step]
                         (if (list? step)
                           `(~(first step) world# ~@(rest step))
                           step)) body))))

(defmacro command [name & body]
  `(def ~name (fn [world# ~@(if (vector? (first body)) (first body) [])]
                (let [steps# (if (vector? (first ~body)) (rest ~body) ~body)]
                  (reduce (fn [world# step#]
                            (if (list? step#)
                              (apply (first step#) world# (rest step#))
                              step#))
                          world#
                          steps#)))))

(defmacro command [name & body]
  `(def ~name (fn [world# ~@(if (vector? (first body)) (first body) [])]
                (let [steps# (if (vector? (first ~body)) (rest ~body) ~body)]
                  (reduce (fn [world# step#]
                            (if (list? step#)
                              (apply (first step#) world# (rest step#))
                              step#))
                          world#
                          steps#)))))

(defmacro command [name & steps]
  `(def ~name
     (fn [world# & args#]
       (let [steps# (list ~@steps)]
         (reduce (fn [world# step#]
                   (apply step# world# args#))
                 world#
                 steps#)))))

(defn find-stack [block block-world]
  (first
   (keep-indexed (fn [idx stack]
                   (when (some #{block} stack) idx))
                 block-world)))

(defn replace-stack [idx new-stack block-world]
  (assoc block-world idx new-stack))

(defn return-blocks [blocks block-world]
  (reduce (fn [world block]
            (let [original-position (- block 1)] ; calculate the original stack of the block
              (update world original-position conj block))) ; return block to its original stack
          block-world
          blocks))

(defn index-of [item coll]
   (some (fn [[idx elem]] (when (= item elem) idx))
        (map-indexed vector coll)))


(defn validate-clear-block [block block-world]
  (when-let [stack-index (find-stack block block-world)]
    (when-let [idx (index-of block (nth block-world stack-index))]
      (if (> idx -1)
        [stack-index (inc idx) (nth block-world stack-index)]
        nil))))

(defn clear-block [block block-world]
  (if-let [[stack-index index-of-block-above stack] (validate-clear-block block block-world)]
    (let [blocks-to-return (subvec stack index-of-block-above)
          updated-world (assoc block-world stack-index (subvec stack 0 index-of-block-above))]
      (return-blocks blocks-to-return updated-world))
    block-world))

(defn clear-src-block [world src-block & r]
  (clear-block src-block world))

(defn clear-dest-block [world _ dest-block]
  (clear-block dest-block world))


(defn place-block [world src-block dest-block]
  (let [src-stack-index (find-stack src-block world)
        dest-stack-index (find-stack dest-block world)]
    (move-block [src-block src-stack-index] dest-stack-index world)))

(defn place-pile [world src-block dest-block]
  (let [src-stack-index (find-stack src-block world)
        dest-stack-index (find-stack dest-block world)]
    (move-pile [src-block src-stack-index] dest-stack-index world)))

(defn reset-world [world & args]
  (mapv vector (range 1 (inc (count world)))))

(command move-onto
  clear-src-block
  clear-dest-block
  place-block)

(command move-over
  clear-src-block
  place-block)

(command pile-onto
  clear-dest-block
  place-pile)

(command pile-over
  place-pile)

(command clear
  clear-src-block)

(command reset
  reset-world)

(comment
  (-> [[1 2] [] [3 4] []]
      (move-onto 2 3)
      (move-onto 4 2)
      (move-over 2 4)
      (move-over 1 4)
      (move-onto 3 4)
      (pile-onto 4 2)
      (move-onto 3 1)
      (pile-over 2 1)
      (clear 3))

  ;; [[1 3] [2] [] [4]]
  ,)
