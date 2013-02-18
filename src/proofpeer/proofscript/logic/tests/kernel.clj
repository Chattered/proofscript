(ns proofpeer.proofscript.logic.tests.kernel
  (:use clojure.test)
  (:use proofpeer.proofscript.logic.kernel)
  (:use proofpeer.proofscript.logic.hol))  

(deftest term-tests
  (let [contextA  :a
        contextB  5
        A-ty      (mk-ty-constr     :At contextA)
        B-ty      (mk-ty-constr     :Bt contextA)
        C-ty      (mk-ty-constr     :Ct contextA)
        D-ty      (mk-ty-constr     :Dt contextB)
        E-ty      (mk-ty-constr     :Et nil)
        F-ty      (mk-ty-constr     :Ft nil)

        f-ty      (mk-fun-ty        A-ty B-ty)
        g-ty      (mk-fun-ty        f-ty C-ty)
        h-ty      (mk-fun-ty        E-ty f-ty)

        A         (mk-var           :A  A-ty)
        B         (mk-var           :B  B-ty)
        C         (mk-var           :C  C-ty)
        D         (mk-var           :D  D-ty)
        E         (mk-var           :E  E-ty)
        F         (mk-var           :F  F-ty)
        A'        (mk-var           :A' A-ty)

        f         (mk-var           :f f-ty)
        g         (mk-var           :g g-ty)
        h         (mk-var           :h h-ty)

        fA        (mk-comb          f A)
        eqAA'     (mk-eq            A A')]
    (do
      ; Context failure
      (is (= nil (mk-ty-constr :foo nil A-ty D-ty)))
      (is (= nil (mk-ty-constr :foo A-ty D-ty)))
      (is (= nil (mk-fun-ty A-ty D-ty)))

      ; Type failures
      (is (= nil (mk-abs  E A)))
      (is (= nil (mk-comb h B)))

      ; Destructions
      (is (= [(eq-term A-ty) A A'] (dest-binop eqAA'))))))
;      (is (= [A A'] (dest-eq eqAA'))))))