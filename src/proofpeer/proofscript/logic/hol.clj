(ns proofpeer.proofscript.logic.hol
   "A context for higher-order logic. There is a boolean type and a constant for equality."
   (:require [proofpeer.proofscript.logic.kernel :as k])
   (:require [clojure.set :as s])
   )

(def alpha-ty
  ^{:private true}
  (k/mk-ty-var 'α))

(def bool-ty
  "The type of booleans, which live in the HOL context."
  (k/mk-ty-constr 'bool :hol))

(defn eq-term
  "Equality on the type ty."
  [ty]
  (k/mk-var '= (k/mk-fun-ty ty (k/mk-fun-ty ty bool-ty))))

(defn mk-eq
  "Given x and y, returns the term x = y."
  [x y]
  (let [x-ty (k/type-of x)]
    (k/mk-binop (eq-term x-ty) x y)))

(defn dest-eq
  "Given an equation, returns the left and right hand side."
  [tm]
  (if-let [[op land rand] (k/dest-binop tm)]
    (when (= :Var (first op))
      (let [[sym ty] (rest op)]
        (when (= '= sym)
          (if-let [[a-ty eqa-ty] (k/dest-fun-ty ty)]
            (if-let [[b-ty bool-ty'] (k/dest-fun-ty eqa-ty)]
              (when (and (= a-ty b-ty)
                         (= bool-ty bool-ty'))
                [land rand]))))))))

(defn REFL
  "Given x, returns ⊦ x = x"
  [x]
  (k/mk-theorem #{} (mk-eq x x)))

(defn MK-COMB
  "Given ⊦ f = g and ⊦ x = y, returns ⊦ f x = g y"
  [fg xy]
  (let [[fgasms fgconcl] (k/dest-theorem fg)]
    (let [[xyasms xyconcl] (k/dest-theorem xy)]
      (if-let [[f g] (dest-eq fgconcl)]
        (if-let [[x y] (dest-eq xyconcl)]
          (k/mk-theorem (s/union xyasms fgasms)
                        (mk-eq (k/mk-comb f x)
                               (k/mk-comb g y))))))))
