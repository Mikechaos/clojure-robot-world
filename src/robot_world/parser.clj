(ns robot-world.parser
  (:require [robot-world.version-2 :refer [initialize-world move pile]]))

(defn- parse-command [command]
  (let [[_ command-type src-block modifier-type dest-block]
        (re-find #"(move|pile) (\d+) (onto|over) (\d+)" command)]
    [(keyword command-type) (Integer. src-block)
     (Integer. dest-block) (keyword modifier-type)]))

(defn- parse-commands [[n & commands]]
  (concat [(Integer. n)] (mapv parse-command commands)))

(defmacro apply-command [command-type# args# world#]
  `(let [command# (~command-type# {:move move :pile pile})]
     (#(apply command# (conj (vec ~args#) %)) ~world#)))

(defn- fn-apply-command [world [command-type & args]]
  (apply-command command-type args world))

(defn- run-commands [[n & commands]]
  (println "Running commands" n commands)
  (let [world (initialize-world n)]
    (reduce fn-apply-command world commands)))

(defn run-program [program]
  (-> program
      (parse-commands)
      (run-commands)))

(comment
  (run-program ["10"
                 "move 3 over 2"
                 "move 8 onto 7"
                 "move 9 onto 7"
                 "move 6 over 8"
                 "pile 7 onto 8"
                 "pile 3 onto 1"
                 "pile 4 onto 1"
                 "move 3 over 2"
                 "pile 2 onto 4"
                 "pile 6 onto 5"
                 "pile 8 over 10"])

  (apply-command :move [3 2 :onto] [[1 2] [] [3 4] [] [5]])
  (initialize-world 10)
  ,)
