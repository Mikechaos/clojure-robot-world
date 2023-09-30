(ns robot-world.helpers)

(defn find-stack [block block-world]
  (first
   (keep-indexed (fn [idx stack]
                   (when (some #{block} stack) idx))
                 block-world)))

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
