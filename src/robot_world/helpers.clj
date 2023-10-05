(ns robot-world.helpers)

(defn find-stack
  "Returns the index of the stack containing the specified block."
  [block block-world]
  (first
   (keep-indexed (fn [idx stack]
                   (when (some #{block} stack) idx))
                 block-world)))

(defn return-blocks
  "Returns the specified blocks to their original stacks."
  [blocks block-world]
  (reduce (fn [world block]
            (let [original-position (- block 1)] ; calculate the original stack of the block
              (update world original-position conj block))) ; return block to its original stack
          block-world
          blocks))

(defn index-of
  "Returns the index of the specified block in the stack, or nil if the block is not in the stack."
  [block stack]
  (some (fn [[idx elem]] (when (= block elem) idx))
        (map-indexed vector stack)))


(defn validate-clear-block
  "Validates the specified block can be cleared, returning the index of the stack containing the block,
   the index of the block above it and the stack itself."
  [block block-world]
  (when-let [stack-index (find-stack block block-world)]
    (when-let [idx (index-of block (nth block-world stack-index))]
      (if (> idx -1)
        [stack-index (inc idx) (nth block-world stack-index)]
        nil))))
