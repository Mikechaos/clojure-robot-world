(ns robot-world.core-test
  (:require [clojure.test :refer :all]
            [robot-world.core :refer :all]))

(deftest test-render-and-commands
  (let [input-program ["10"
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
                       "pile 8 over 10"
                       "quit"]
        expected-output "1: 1 4 2 3\n2: \n3: \n4: \n5: 5 6\n6: \n7: \n8: \n9: \n10: 10 8 7 9"]
    (execute-command initialize-world (Integer. (first input-program)))
    (let [parsed-program (parse-program (rest input-program))]
      (execute-parsed-program parsed-program))
    (is (= (with-out-str (render (nth @block-world-history @current-state-index))) expected-output))))
