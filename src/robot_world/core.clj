(ns robot-world.core
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

;; Data Structures
(def block-world-history (atom [(vec (repeat 3 []))]))
(def current-state-index (atom 0))

;; Helper Functions
(defn some-indexed [pred coll]
  (some (fn [[idx item]] (when (pred item) idx))
        (map-indexed vector coll)))

(defn find-stack [block block-world] 
  (some-indexed #(some #{block} %2) block-world))


(defn replace-stack [idx new-stack block-world]
  (assoc block-world idx new-stack))

(defn index-of [item coll]
  (loop [i 0
         coll-seq coll]
    (cond
      (empty? coll-seq) -1
      (= item (first coll-seq)) i
      :else (recur (inc i) (rest coll-seq)))))

;; Macros
(defmacro defcommand [name & body]
  `(defn ~name [current-block-world# & args#]
     (println "Executing command:" '~name args#)
     (let [new-block-world# (apply (fn [current-block-world# & args#] ~@body) current-block-world# args#)]
       new-block-world#)))

(defmacro with-blocks-on-top [block & body]
  `(let [stack (on-top-of ~block block-world)
         blocks-on-top (rest stack)]
     (return-blocks blocks-on-top block-world)
     ~@body
     (reduce (fn [bw block]
               (let [original-pos (find-stack block bw)]
                 (replace-stack original-pos (conj (get bw original-pos) ~block) bw)))
             block-world
             blocks-on-top)))

(defmacro remove-block [block stack-index]
  `(let [current-state# (nth @block-world-history @current-state-index)
         block-index# (index-of ~block (nth current-state# ~stack-index))]
     (when (not= block-index# -1)
       (let [updated-stack# (vec (remove #{~block} (nth current-state# ~stack-index)))]
         (assoc-in current-state# [~stack-index] updated-stack#)))))

(defmacro move-block-to [src dest blocks-on-dest]
  `(let [pos-src (find-stack ~src)
         pos-dest (find-stack ~dest)]
     (replace-stack pos-src [])
     (replace-stack pos-dest (concat [~dest] (on-top-of ~src) ~blocks-on-dest))))

(defmacro return-blocks-on-top [a]
  `(let [stack# (find-stack ~a)
         idx# (index-of ~a stack#)]
     (doseq [block# (take-last (- (count stack#) idx#) stack#)]
       (move-block-to-new-stack block#))))

(defmacro add-block-to-stack [block stack-index]
  `(let [current-state# (nth @block-world-history @current-state-index)
         updated-stack# (conj (nth current-state# ~stack-index) ~block)]
     (assoc-in current-state# [~stack-index] updated-stack#)))

(defmacro place-block-on [a b]
  `(let [stack-a# (find-stack ~a)
         stack-b# (find-stack ~b)]
     (remove-block ~a stack-a#)
     (add-block-to-stack ~a stack-b#)))

(defmacro move-block [a b]
  `(do
     (let [stack-a# (find-stack ~a)
           stack-b# (find-stack ~b)]
       (remove-block ~a stack-a#)
       (add-block-to-stack ~a stack-b#))))

(defmacro place-pile-on [pile b]
  `(let [stack-b# (find-stack ~b)]
     (concat stack-b# ~pile)))

(defmacro remove-pile [a]
  `(let [stack-a# (find-stack ~a)
         idx-a# (index-of ~a stack-a#)]
     (subvec stack-a# idx-a#)))

(defmacro sub-stack-from [block stack]
  `(let [block-index# (index-of ~block ~stack)]
     (subvec ~stack block-index#)))

(defmacro get-pile [block]
  `(let [current-state# (nth @block-world-history @current-state-index)
         stack-index# (find-stack ~block current-state#)]
     (when (not= stack-index# -1)
       (sub-stack-from ~block (nth current-state# stack-index#)))))

(defmacro move-block-to-new-stack [block]
  `(do
     (let [stack# (find-stack ~block)
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
  
;; Command Definitions
(defcommand initialize-world [[] n]
  (map list (range 1 (inc n))))

(defcommand move-onto [a b]
  (return-blocks-on-top a)
  (return-blocks-on-top b)
  (move-block a b))

(defcommand move-over [a b]
  (return-blocks-on-top a)
  (place-block-on a b))

(defcommand pile-onto [a b]
  (return-blocks-on-top b)
  (let [pile (get-pile a)]
    (remove-pile a)
    (place-pile-on pile b)))

(defcommand pile-over [a b]
  (let [pile (get-pile a)]
    (remove-pile a)
    (place-pile-on pile b)))

;; Execution Function
;; (defn execute-command [command & args]
;;   (let [current-block-world (nth @block-world-history @current-state-index)]
;;     (cond (or (= command 'undo) (= command 'redo)) (apply command args)
;;           (= command initialize-world)
;;           (do
;;             (reset! block-world-history [(apply command args)])
;;             (reset! current-state-index 0))
;;           :else
;;           (apply command (cons current-block-world args)))))

(defn execute-command [command & args]
  (let [current-block-world (nth @block-world-history @current-state-index)]
    (cond
      (= command initialize-world)
      (do
        (reset! block-world-history (apply command args))
        (reset! current-state-index 0))

      (= command 'undo) (apply command args)
      (= command 'redo) (apply command args)

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
