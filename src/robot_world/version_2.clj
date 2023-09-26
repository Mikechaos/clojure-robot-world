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
  (loop [i 0
         coll-seq coll]
    (cond
      (empty? coll-seq) -1
      (= item (first coll-seq)) i
      :else (recur (inc i) (rest coll-seq)))))

(defn clear-block [block block-world]
  (let [stack-index (find-stack block block-world)]
    (if (= stack-index -1)
      block-world
      (let [stack (nth block-world stack-index)
            idx (index-of block stack)]
        (if (= idx -1)
          block-world
          (let [blocks-to-return (subvec stack (inc idx))
                updated-world (assoc block-world stack-index (subvec stack 0 (inc idx)))]
            (return-blocks blocks-to-return updated-world)))))))

(defn log-illegal-move-and-return [command-type src-block dest-block modifier-type block-world]
  (println
   "Performing" command-type
   src-block modifier-type dest-block
   "- Invalid move"
   "in world" block-world)
  block-world)

(defn log-legal-move-and-return [command-type src-block dest-block modifier-type block-world]
  (println
   "Performing" command-type
   src-block modifier-type dest-block
   " in " block-world " to get " block-world)
  block-world)

(defn move-block [[src-block src-stack-index] dest-stack-index block-world]
      (let [updated-src-stack (vec (remove #{src-block} (nth block-world src-stack-index)))
            updated-world (assoc block-world src-stack-index updated-src-stack)]
        (assoc updated-world dest-stack-index (conj (nth updated-world dest-stack-index) src-block))))

(defn move-pile [[src-block src-stack-index] dest-stack-index block-world]
  (let [src-stack (nth block-world src-stack-index)
        dest-stack (nth block-world dest-stack-index)
        block-index-in-stack (index-of src-block src-stack)
        blocks-pile (subvec src-stack block-index-in-stack)
        remaining-blocks-on-src-stack (subvec src-stack 0 block-index-in-stack)]
    (-> block-world
        (assoc src-stack-index remaining-blocks-on-src-stack)
        (assoc dest-stack-index (vec (concat dest-stack blocks-pile))))))

(defn validate-world-before-command [src-block dest-block block-world]
  (let [src-stack-index (find-stack src-block block-world)
        dest-stack-index (find-stack dest-block block-world)]
    (if (or (or (nil? src-stack-index) (nil? dest-stack-index)) (= src-stack-index dest-stack-index))
      [false "invalid move"]
      [src-stack-index dest-stack-index])))

(defn move [src-block dest-block move-type block-world]
  (let [[src-stack-index dest-stack-index]
        (validate-world-before-command src-block dest-block block-world)]
    (if (false? src-stack-index)
      (log-illegal-move-and-return "move" src-block dest-block move-type block-world)
      (let [cleared-world (condp = move-type
                            :onto (->> block-world
                                       (clear-block src-block)
                                       (clear-block dest-block))
                            :over (clear-block src-block block-world))
            new-world (move-block [src-block src-stack-index] dest-stack-index cleared-world)]
          (log-legal-move-and-return "move" src-block dest-block move-type new-world)))))

(defn pile [src-block dest-block pile-type block-world]
  (let [[src-stack-index dest-stack-index]
        (validate-world-before-command src-block dest-block block-world)]
    (if (false? src-stack-index)
      (log-illegal-move-and-return "pile" src-block dest-block pile-type block-world)
      (let [cleared-world (condp = pile-type
                            :onto (->> block-world
                                       (clear-block dest-block))
                            :over block-world)
            new-world (move-pile [src-block src-stack-index] dest-stack-index cleared-world)]
          (log-legal-move-and-return "pile" src-block dest-block pile-type new-world)))))
