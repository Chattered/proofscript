(ns proofpeer.proofscript.logic.term-utils
  (:use proofpeer.proofscript.logic.term))

;;; Term and type utilities.

(defn nary
  "nary [x y .. z] returns the type x -> y -> ... -> z"
  ([ x ] x)
  ([ x & args ] (mk-fun-ty x (apply nary args))))
  

(defn mk-binop
  "mk-binop op x y returns the term (op x) y"
  [op x y]
  (mk-comb (mk-comb op x) y))

(defn dest-binop
  "Returns the lhs and rhs of the named binary operation, or nil if the term is not such an application."
  [op tm]
  (if-let [[rator rhs] (dest-comb tm)]
          (if-let [[op lhs] (dest-comb rator)]
                  [lhs rhs])))
