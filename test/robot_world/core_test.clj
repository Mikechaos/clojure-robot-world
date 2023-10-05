(ns robot-world.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [robot-world.core :as core]
            [robot-world.version-2 :as v2]
            [robot-world.parser :refer [run-program]]
            [robot-world.dsl2 :as dsl2]
            [robot-world.dsl-v1 :as dsl1]))

(deftest test-find-stack
  (testing "find-stack should return the correct stack index"
    (is (= (core/find-stack 3 (core/initialize-world 5)) 2)))

  (testing "find-stack should return the correct stack index when blocks are moved"
    (is (= (core/find-stack 3 [[1] [2 3] [] [4] [5]]) 1)))

  (testing "find-stack should return nil if the block is not present in any stack"
    (is (nil? (core/find-stack 6 (core/initialize-world 5))))))

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


;; Failing test case as the dsl-v1 didn't support no-op for invalid commands
(deftest test-dsl-v1
  (testing "Test the DSL v1"
    (let [initial-world (v2/initialize-world 4)
          updated-world (-> initial-world
                            (dsl1/move-onto 2 3)
                            (dsl1/move-onto 4 2)
                            (dsl1/move-over 2 4)
                            (dsl1/move-over 1 4)
                            (dsl1/move-onto 3 4)
                            (dsl1/pile-onto 4 2)
                            (dsl1/move-onto 3 1)
                            (dsl1/pile-over 2 1)
                            (dsl1/clear 3))]
      (is (= updated-world [[1] [2] [3] [4]])))))

(deftest test-dsl-v2
  (testing "Test the DSL v2"
    (let [initial-world (v2/initialize-world 4)
          updated-world (-> initial-world
                            (dsl2/move-onto 2 3)
                            (dsl2/move-onto 4 2)
                            (dsl2/move-over 2 4)
                            (dsl2/move-over 1 4)
                            (dsl2/move-onto 3 4)
                            (dsl2/pile-onto 4 2)
                            (dsl2/move-onto 3 1)
                            (dsl2/pile-over 2 1))]
      (is (= updated-world [[] [] [3 2 4 1] []])))))
