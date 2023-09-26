(ns robot-world.core
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

;; Data Structures
(def block-world-history (atom [(vec (repeat 3 []))]))
(def current-state-index (atom 0))

;; Helper Functions
(defn some-indexed [pred coll]
  (some (fn [pair]
          (let [[idx item] pair]
            (when (pred idx item) idx)))
        (map-indexed vector coll)))

(defn find-stack [block block-world]
  (some-indexed (fn [idx stack]
                  (when (some #{block} stack) idx))
                block-world))


(defn replace-stack [idx new-stack block-world]
  (assoc block-world idx new-stack))

(defn index-of [item coll]
  (loop [i 0
         coll-seq coll]
    (cond
      (empty? coll-seq) -1
      (= item (first coll-seq)) i
      :else (recur (inc i) (rest coll-seq)))))

(defn on-top-of [block block-world]
  (let [stack-index (find-stack block block-world)]
    (when (not= stack-index -1)
      (nth block-world stack-index))))

(defn return-blocks [blocks block-world]
  (reduce (fn [bw block]
            (let [original-pos (find-stack block bw)]
              (replace-stack original-pos (conj (bw original-pos) block) bw)))
          block-world
          blocks))

(defn return-blocks [blocks block-world]
  (reduce (fn [world block]
            (let [original-position (- block 1)] ; calculate the original stack of the block
              (update world original-position conj block))) ; return block to its original stack
          block-world
          blocks))

;; Macros
(defmacro defcommand [name args & body]
  `(defn ~name ~args
     (println "Executing command:" '~name ~args)
     (let [new-block-world# (do ~@body)]
       new-block-world#)))

(defmacro with-blocks-on-top [current-block-world block & body]
  `(let [stack# (on-top-of ~block ~current-block-world)
         blocks-on-top# (rest stack#)]
     (return-blocks blocks-on-top# ~current-block-world)
     ~@body
     (reduce (fn [bw# block#]
               (let [original-pos# (find-stack block# bw#)]
                 (replace-stack original-pos# (conj (get bw# original-pos#) ~block) bw#)))
             ~current-block-world
             blocks-on-top#)))

(defmacro remove-block [current-block-world block stack-index]
  `(let [block-index# (index-of ~block (nth ~current-block-world ~stack-index))]
     (when (not= block-index# -1)
       (let [updated-stack# (vec (remove #{~block} (nth ~current-block-world ~stack-index)))]
         (assoc-in ~current-block-world [~stack-index] updated-stack#)))))

(defmacro move-block-to [current-block-world src dest blocks-on-dest]
  `(let [pos-src (find-stack ~src ~current-block-world)
         pos-dest (find-stack ~dest ~current-block-world)]
     (replace-stack pos-src [] ~current-block-world)
     (replace-stack pos-dest (concat [~dest] (on-top-of ~src ~current-block-world) ~blocks-on-dest) ~current-block-world)))

(defmacro return-blocks-on-top [current-block-world a]
  `(let [stack# (find-stack ~a ~current-block-world)
         idx# (index-of ~a stack#)]
     (doseq [block# (take-last (- (count stack#) idx#) stack#)]
       (move-block-to-new-stack ~current-block-world block#))))

(defmacro add-block-to-stack [current-block-world block stack-index]
  `(let [updated-stack# (conj (nth ~current-block-world ~stack-index) ~block)]
     (assoc-in ~current-block-world [~stack-index] updated-stack#)))

(defmacro place-block-on [current-block-world a b]
  `(let [stack-a# (find-stack ~a ~current-block-world)
         stack-b# (find-stack ~b ~current-block-world)]
     (remove-block ~current-block-world ~a stack-a#)
     (add-block-to-stack ~current-block-world ~a stack-b#)))

(defmacro move-block [current-block-world a b]
  `(do
     (let [stack-a# (find-stack ~a ~current-block-world)
           stack-b# (find-stack ~b ~current-block-world)]
       (remove-block ~current-block-world ~a stack-a#)
       (add-block-to-stack ~current-block-world ~a stack-b#))))

(defmacro place-pile-on [current-block-world pile b]
  `(let [stack-b# (find-stack ~b ~current-block-world)]
     (concat stack-b# ~pile)))

(defmacro remove-pile [current-block-world a]
  `(let [stack-a# (find-stack ~a ~current-block-world)
         idx-a# (index-of ~a stack-a#)]
     (subvec stack-a# idx-a#)))

(defmacro sub-stack-from [current-block-world block stack]
  `(let [block-index# (index-of ~block ~stack)]
     (subvec ~stack block-index#)))

(defmacro get-pile [current-block-world block]
  `(let [stack-index# (find-stack ~block ~current-block-world)]
     (when (not= stack-index# -1)
       (sub-stack-from ~current-block-world ~block (nth ~current-block-world stack-index#)))))

(defmacro move-block-to-new-stack [current-block-world block]
  `(do
     (let [stack# (find-stack ~block ~current-block-world)
           idx# (index-of ~block stack#)]
       (swap! block-world-history update-in [stack#] #(subvec % 0 idx#)))
     (swap! block-world-history conj [(list ~block)])))

(defmacro undo []
  `(when (> @current-state-index 0)
     (swap! current-state-index dec)
     (nth @block-world-history @current-state-index)))

(defmacro redo []
  `(when (< @current-state-index (dec (count @block-world-history)))
     (swap! current-state-index inc)
     (nth @block-world-history @current-state-index)))

;; ... [Update other macros similarly] ...

;; Command Definitions
(defcommand initialize-world [n]
  (mapv vector (range 1 (inc n))))

(defcommand move-onto [current-block-world a b]
  (println "block-world before move-onto:" current-block-world)
  (with-blocks-on-top current-block-world a
    (with-blocks-on-top current-block-world b
      (move-block current-block-world a b))))

(defcommand move-over [current-block-world a b]
  (return-blocks-on-top current-block-world a)
  (place-block-on current-block-world a b))

(defcommand pile-onto [current-block-world a b]
  (return-blocks-on-top current-block-world b)
  (let [pile (get-pile current-block-world a)]
    (remove-pile current-block-world a)
    (place-pile-on current-block-world pile b)))

(defcommand pile-over [current-block-world a b]
  (let [pile (get-pile current-block-world a)]
    (remove-pile current-block-world a)
    (place-pile-on current-block-world pile b)))
;; ... [Update other command definitions similarly] ...

;; Parse Commands

(defn parse-command [input]
  (case (first input)
    "move" (let [a (symbol (second input))
                 b (symbol (nth input 3))
                 move-fn (if (= "over" (nth input 2)) 'move-over 'move-onto)]
             #(move-fn a b))
    "pile" (let [a (symbol (second input))
                 b (symbol (nth input 3))
                 move-fn (if (= "over" (nth input 2)) 'move-over 'move-onto)]
             #(move-fn a b))
    "quit" 'quit
    "undo" 'undo
    "redo" 'redo
    (throw (IllegalArgumentException. (str "No matching clause: " (first input))))))

(defn parse-program [input-program]
  (map (fn [line]
         (parse-command (clojure.string/split line #" ")))
       input-program))


;; Execution Function
(defn execute-command [command & args]
  (let [current-block-world (nth @block-world-history @current-state-index)]
    (cond
      (= command initialize-world)
      (do
        (println "Initializing world." command args)
        (reset! block-world-history [(apply command args)])
        (reset! current-state-index 0))

      (= command 'undo) (undo)
      (= command 'redo) (redo)

      :else
      (let [new-block-world (apply command current-block-world args)]
        (swap! block-world-history conj new-block-world)
        (swap! current-state-index inc)))))

;; (defn execute-command [command & args]
;;   (if (= command initialize-world)
;;     (do
;;       (println "Initializing world." command args)
;;       (reset! block-world-history [(apply command args)])
;;       (reset! current-state-index 0))
;;     (let [current-block-world (nth @block-world-history @current-state-index)
;;           new-block-world (apply command current-block-world args)]
;;       (swap! block-world-history conj new-block-world)
;;       (swap! current-state-index inc))))

(defn execute-parsed-command [command]
  (println "Executing parsed command:" command)
  (cond
    (= command 'quit) (println "Quitting program.")
    (= command 'undo) (undo)
    (= command 'redo) (redo)
    :else (do
            (command)
            (println "Block world after command:" @block-world-history @current-state-index))))

(defn execute-parsed-program [parsed-program]
  (doseq [command parsed-program]
    (execute-parsed-command command)))

;; Render Function
(defn render [block-world]
  (doseq [[idx stack] (map-indexed vector block-world)]
    (println (str (inc idx) ":") (str/join " " stack))))
