(ns robot-world.core-test
  (:require [clojure.test :refer :all]
            [robot-world.core :refer :all]
            [robot-world.version-2 :as v2]
            [robot-world.parser :refer [run-program]]))

(deftest test-find-stack
  (testing "find-stack should return the correct stack index"
    (is (= (find-stack 3 (initialize-world 5)) 2)))

  (testing "find-stack should return the correct stack index when blocks are moved"
    (is (= (find-stack 3 [[1] [2 3] [] [4] [5]]) 1)))

  (testing "find-stack should return nil if the block is not present in any stack"
    (is (nil? (find-stack 6 (initialize-world 5))))))

(deftest test-move-v2
  (let [initial-world [[] [2] [3 1] [4]]]

    (testing (str "move-onto command - moving 2 onto 3 from " initial-world)
      (let [updated-world (v2/move 2 3 :onto initial-world)
            expected-world [[1] [] [3 2] [4]]]
        (is (= updated-world expected-world))))

    (testing "move-over command"
      (let [updated-world (v2/move 2 3 :over initial-world)
            expected-world [[] [] [3 1 2] [4]]]
        (is (= updated-world expected-world))))

    (testing "multiple chained move onto and move over commands"
      (let [updated-world (->> initial-world
                                (v2/move 2 3 :onto)
                                (v2/move 1 3 :over)
                                (v2/move 4 2 :onto)
                                (v2/move 3 2 :over))
            expected-world [[1] [] [3 2 4] []]]
        (is (= updated-world expected-world)))
      (testing "multiple chained move onto and move over commands with a specific final configuration"
        (let [updated-world (->> initial-world
                                 (v2/move 2 3 :onto)
                                 (v2/move 4 3 :over)
                                 (v2/move 1 2 :onto)
                                 (v2/move 3 4 :over)
                                 (v2/move 4 1 :onto)
                                 (v2/move 3 1 :over)
                                 (v2/move 2 3 :over))
              expected-world [[1 4 3 2] [] [] []]]
          (is (= updated-world expected-world)))))))

(deftest test-pile-v2
  (let [initial-world [[1 2] [] [3 4] [] [5]]]

    (testing "pile-onto command"
      (let [updated-world (v2/pile 3 1 :onto initial-world)
            expected-world [[1 3 4] [2] [] [] [5]]]
        (is (= updated-world expected-world))))

    (testing "pile-over command"
      (let [updated-world (v2/pile 3 1 :over initial-world)
            expected-world [[1 2 3 4] [] [] [] [5]]]
        (is (= updated-world expected-world))))

    (testing "multiple chained commands"
      (let [updated-world (->> initial-world
                               (v2/pile 4 1 :onto)
                               (v2/pile 3 2 :over)
                               (v2/pile 2 4 :onto)
                               (v2/move 5 1 :over))
            expected-world [[1 4 2 3 5] [] [] [] []]]
        (is (= updated-world expected-world))))
    (testing "no-op commands when block a and b are on the same stack"
      (let [initial-no-op-world [[0 4 2 5] [1] [3] [] []]
            final-no-op-world (->> initial-no-op-world
                                  (v2/pile 4 2 :onto)
                                  (v2/pile 3 3 :over)
                                  (v2/pile 4 0 :onto))]
        (is (= final-no-op-world initial-no-op-world))))))

(deftest test-input-program
  (testing "Test parsing and executing the input program"
    (let [program ["10"
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
                   "pile 8 over 10"]
          world (run-program program)
          expected-output [[1 4 2 3] [] [] [] [5 6] [] [] [] [] [10 8 7 9]]]
      (is (= world expected-output)))))
