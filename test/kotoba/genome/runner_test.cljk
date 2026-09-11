(ns kotoba.genome.runner-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.genome.runner :as runner]))

(def ready-adapter
  {:job.adapter/id :runner/fastqc
   :job.adapter/status :ready
   :job.adapter/software :sw/fastqc
   :job.adapter/operation :op/qc
   :job.adapter/command {:command/argv ["fastqc" "a.fastq" "-o" "out/qc"]}})

(def missing-adapter
  (assoc ready-adapter :job.adapter/status :missing-inputs))

(deftest ready?-reflects-adapter-status
  (is (true? (runner/ready? ready-adapter)))
  (is (false? (runner/ready? missing-adapter))))

(deftest argv-extracts-the-command-vector
  (is (= ["fastqc" "a.fastq" "-o" "out/qc"] (runner/argv ready-adapter))))

(deftest dry-run-never-executes-and-reports-argv
  (testing "dry-run just describes the intended command, it does not run anything"
    (let [res (runner/dry-run ready-adapter)]
      (is (= :dry-run (:run/status res)))
      (is (= :runner/fastqc (:run/adapter res)))
      (is (= :sw/fastqc (:run/tool res)))
      (is (= ["fastqc" "a.fastq" "-o" "out/qc"] (:run/argv res))))))

(deftest execute!-defaults-to-dry-run-when-exec-is-not-explicitly-enabled
  (testing "without KOTOBA_RUNNER_EXEC=1 in the environment, execute! never shells out"
    (is (nil? (System/getenv "KOTOBA_RUNNER_EXEC")))
    (is (= :dry-run (:run/status (runner/execute! ready-adapter))))))

(deftest execute!-refuses-non-whitelisted-executables
  (testing "an adapter whose argv[0] is not in the whitelist throws instead of running"
    (let [rogue (assoc-in ready-adapter [:job.adapter/command :command/argv] ["rm" "-rf" "/"])]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not whitelisted"
                             (runner/execute! rogue))))))

(deftest execute!-accepts-whitelisted-executables
  (testing "whitelisted executables (clojure, fastqc, bwa) are allowed through to the dry-run path"
    (is (= :dry-run (:run/status (runner/execute! ready-adapter))))
    (let [bwa (assoc-in ready-adapter [:job.adapter/command :command/argv] ["bwa" "mem" "ref.fa" "reads.fq"])]
      (is (= :dry-run (:run/status (runner/execute! bwa)))))))
