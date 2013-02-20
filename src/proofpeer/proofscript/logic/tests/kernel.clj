(ns proofpeer.proofscript.logic.tests.kernel
  (:use clojure.test)
  (:use proofpeer.proofscript.logic.kernel))

(def contextA  5)
(def A-ty      (mk-ty-constr     :At :hol))
(def B-ty      (mk-ty-constr     :Bt :hol))
(def C-ty      (mk-ty-constr     :Ct :hol))
(def D-ty      (mk-ty-constr     :At contextA))
(def E-ty      (mk-ty-constr     :Et nil))
(def F-ty      (mk-ty-constr     :Ft nil))

(def f-ty      (mk-fun-ty        A-ty B-ty))
(def g-ty      (mk-fun-ty        f-ty C-ty))
(def h-ty      (mk-fun-ty        E-ty f-ty))

(def A         (mk-var           :A  A-ty))
(def B         (mk-var           :B  B-ty))
(def C         (mk-var           :C  C-ty))
(def D         (mk-var           :D  D-ty))
(def E         (mk-var           :E  E-ty))
(def F         (mk-var           :F  F-ty))
(def A'        (mk-var           :A' A-ty))

(def f         (mk-var           :f f-ty))
(def g         (mk-var           :g g-ty))
(def h         (mk-var           :h h-ty))

(def fA        (mk-comb          f A))

(def eqAA'     (mk-eq            A A'))

(deftest term-test
  ;; Context failure
  (is (= nil (mk-ty-constr :foo nil A-ty D-ty)))
  (is (= nil (mk-ty-constr :foo A-ty D-ty)))
  (is (= nil (mk-fun-ty A-ty D-ty)))
  
  ;; Type failures
  (is (= nil (mk-abs  E A)))
  (is (= nil (mk-comb h B)))

  ;; Destructions
  (is (= [(eq-term A-ty) A A'] (dest-binop eqAA')))
  (is (= [A A'] (dest-eq eqAA')))
  (is (= [A A] (dest-eq (second (dest-theorem (REFL A)))))))