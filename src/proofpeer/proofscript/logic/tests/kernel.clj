(ns proofpeer.proofscript.logic.tests.kernel
  (:use clojure.test)
  (:use proofpeer.proofscript.logic.kernel))

(deftest test-ty-constructors-and-destructors
  (is (ty-set? ty-set))
  (is (ty-bool? ty-bool))
  (is (not (ty-set? ty-bool)))
  (is (not (ty-bool? ty-set)))
  (is (= (ty-fun? (ty-fun ty-set ty-bool)) [ty-set ty-bool]))
  (is (not (or (ty-fun? ty-bool) (ty-fun? ty-set))))
  (is (= (ty-var? (ty-var "a")) "a"))
  (is (not (ty-var? ty-set))))

(deftest test-ty-collect-tyvars
  (is (= (ty-collect-tyvars ty-set) #{}))
  (is (= (ty-collect-tyvars (ty-fun (ty-var "a") (ty-var "b"))) #{"a" "b"}))
  (is (= (ty-collect-tyvars (ty-fun (ty-var "a") (ty-var "a"))) #{"a"})))

(deftest test-ty-polymorphic
  (is (not (ty-polymorphic? ty-bool)))
  (is (not (ty-polymorphic? ty-set)))
  (is (ty-polymorphic? (ty-fun (ty-fun ty-set (ty-var "a")) ty-bool))))




