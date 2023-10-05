(ns robot-world.dsl-v1)

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

(defn clear-src-block [world src-block & _]
  (clear-block src-block world))

(defn clear-dest-block [world _ dest-block]
  (clear-block dest-block world))

(defn move-pile [[src-block src-stack-index] dest-stack-index block-world]
  (let [src-stack (nth block-world src-stack-index)
        block-index-in-stack (index-of src-block src-stack)
        blocks-pile (subvec src-stack block-index-in-stack)
        updated-world (assoc block-world
                             src-stack-index (subvec src-stack 0 block-index-in-stack)
                             dest-stack-index (into (nth block-world dest-stack-index) blocks-pile))]
    updated-world))

(defn move-to [world src-block dest-block]
  (let [src-stack-index (find-stack src-block world)
        dest-stack-index (find-stack dest-block world)]
  (move-pile [src-block src-stack-index] dest-stack-index world)))

(defn reset-world [world & _]
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
