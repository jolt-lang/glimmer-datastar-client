(ns jolt.datastar.client.test-runner
  "Entry point for `jolt -M:test`: runs every test namespace, exits non-zero on a
  failure or a namespace that fails to load."
  (:require [clojure.test :as t]))

(defn -main [& _]
  (let [namespaces '[jolt.datastar.client.signals-test
                     jolt.datastar.client.syntax-test
                     jolt.datastar.client.store-test
                     jolt.datastar.client.expr-test
                     jolt.datastar.client.attrs-test
                     jolt.datastar.client.protocol-test
                     jolt.datastar.client.client-test
                     jolt.datastar.client.e2e-test
                     jolt.datastar.client.json-e2e-test]
        load-errors (count (for [ns namespaces
                                 :let [e (try (require ns) nil (catch Exception e e))]
                                 :when e]
                             (println "ERROR requiring" ns ":" (ex-message e))))
        results (apply t/run-tests namespaces)
        failed (+ (:fail results 0) (:error results 0) load-errors)]
    (println "----")
    (println "tests:" (:test results 0) "assertions:" (:pass results 0) "passed /" failed "failed")
    (shutdown-agents)
    (when (pos? failed) (System/exit 1))))
