(ns proofpeer.proofscript.logic.hol
   "A context for higher-order logic. There is a boolean type and a constant for equality."
   (:use proofpeer.proofscript.logic.kernel)
)

(def alpha-ty
  ^{:private true}
  (mk-ty-var 'α))

(def bool-ty
  "The type of booleans, which live in the HOL context."
  (mk-ty-constr 'bool :hol))

(defn eq-term
  "Equality on the type ty."
  [ty]
  (mk-var '= (mk-fun-ty ty (mk-fun-ty ty bool-ty))))

(defn mk-eq
  "Given x and y, returns the term x = y."
  [x y]
  (let [x-ty (type-of x)]
    (mk-binop (eq-term x-ty) x y)))

(defn dest-eq
  "Given an equation, returns the left and right hand side."
  [tm]
  (if-let [[op land rand] (dest-binop tm)]
    (when (= :Var (first op))
      (let [[sym ty] (rest op)]
        (when (= '= sym)
          (if-let [[a-ty eqa-ty] (dest-fun-ty ty)]
            (if-let [[b-ty bool-ty'] (dest-fun-ty eqa-ty)]
              (when (and (= a-ty b-ty)
                         (= bool-ty bool-ty'))
                [land rand]))))))))

(defn refl
  "Given x, returns ⊦ x = x"
  [x]
  (mk-theorem [] (mk-eq x x)))
