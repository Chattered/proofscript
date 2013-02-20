(ns proofpeer.proofscript.logic.kernel
  "The proof peer kernel. To guarantee
  logical soundness, all data should be constructed from the exported
  functions. These functions, and all derived functions, make for a *very* thin
  abstraction over the underlying data. Standing working hypothesis is that
  that all circumventions of the standard interface entail a reformat of the
  universe.

  The basic syntax of the kernel supports type variables, type constructors,
  variables, constants, combinations and abstractions. However, the arguments
  to all type constructors must have kind *. Additionally, all types are
  indexed by a context, which is used to associate axioms and definitions with
  types, terms and theorems. The context is an arbitrary value such that types
  with distinct context are taken to be distinct.

  All contexts are greater than the nil context and otherwise incomparable. See
  the documentation for mk-ty-constr."

  (:use phlegmaticprogrammer.clojure_util.core)
  (:require [clojure.set :as s])
  )

(defmacro cond-let
  ([] nil)
  ([clause & rest]
     (if-let [[bnd expr body] clause]
       `(if-let [~bnd ~expr]
          ~body
          (cond-let ~@rest)))))

(defn is-term?
  "Returns true if tm is a term."
  [tm]
  #{:Var :Const :Comb :Abs} (first tm))

(defn dest-var
  "Returns [v ty] for a variable and nil for any other term."
  [term]
  (assert is-term? term)
  (let [[tag name ty] term]
    (when (= tag :Var)
      [name ty])))

(defn dest-const
  "Returns [c ty] for a constant and nil for any other term."
  [term]
  (assert is-term? term)
  (let [[tag name ty] term]
    (when (= tag :Const)
      [name ty])))

(defn dest-comb
  "Returns [rator rand] for a combination and nil for any other term."
  [term]
  (assert is-term? term)
  (let [[tag rator rand] term]
    (when (= tag :Comb)
      [rator rand])))

(defn dest-abs
  "Returns [bound-var body] for an abstraction and nil for any other term."
  [term]
  (assert is-term? term)
  (let [[tag bound-var body] term]
    (when (= tag :Abs)
      [bound-var body])))

(defn is-type?
  "Returns true if ty is a type."
  [ty]
  (#{:TypeVar :TypeConstr} (first ty)))

(defn dest-ty-var
  "Returns the name of a type variable and nil for a value of a type constructor."
  [ty]
  (assert (is-type? ty))
  (let [[tag name] ty]
    (when (= tag :TypeVar)
      name)))

(defn dest-ty-constr
  "Returns [constr context arg1 arg2 ... argn] for a value of a type constructor."
  [ty]
  (assert (is-type? ty))
  (let [[tag & thety] ty]
    (when (= tag :TypeConstr)
      thety)))

(defn context-of-ty
  "Returns the context of a type."
  [ty]
  (assert (is-type? ty))
  (cond-let
   [[v ty] (dest-ty-var ty) nil]
   [[_ context] (dest-ty-constr ty) context]))

(defn mk-ty-constr
  "Apply a type constructor in a context. The context of the returned type is
the maximum of the given context and the contexts of all type argument
contexts. Returns nil if no such maximum exists."
  [cons context & args]
  (let [[maxContext valid]
        (reduce #(let [[maxC valid] %1
                        cx          (context-of-ty %2)] 
                   (and valid
                        (if (and maxC cx)
                          (if (= maxC cx)
                            [maxC true]
                            [nil  false])
                          [(or maxC cx) true])))
                [context true] args)]
    (when valid
      [:TypeConstr cons maxContext args])))

(defn mk-ty-var
  "Create a type variable."
  [sym]
  [:TypeVar sym])

(defn mk-fun-ty
  "Apply the function type constructor, which lives in the nil context."
  [ty-dom ty-codom]
  (mk-ty-constr '-> nil ty-dom ty-codom))

(defn dest-fun-ty
  "Returns the rator type and rand type of a function type in a vector, or nil if
the argument is not a function type."
  [ty]
  (if-let [[cons context args] (dest-ty-constr ty)]
    (when (= cons '->)
      (if (= (count args) 2)
        args))))
  
(defn type-of
  "Returns the type of a term."
  [tm]
  (cond-let
   [[_ ty] (dest-var tm) ty]
   [[_ ty] (dest-const tm) ty]
   [[rator rand] (dest-comb tm) (second (dest-fun-ty (type-of rator)))]
   [[bound-var body] (dest-abs tm) (mk-fun-ty (type-of bound-var) (type-of body))]))

(defn mk-var
  "Construct a typed variable."
  [sym type]
  (assert is-type? type)
  [:Var sym type])

(defn mk-const
  "Construct a typed constant."
  [sym type]
  (assert is-type? type)
  [:Const sym type])

(defn mk-comb
  "Construct a typed combination. Returns nil if the rator and rand types do not agree."
  [rator rand]
  (if-let [[dom-ty _] (dest-fun-ty (type-of rator))]
    (when (= dom-ty (type-of rand))
      [:Comb rator rand])))

(defn mk-abs
  "Construct a typed abstraction. Returns nil if the variable is not in the
same context as the body."
  [bndV body]
  (let [[sym ty] (dest-var bndV)]
    (when (= (context-of-ty ty) (context-of-ty (type-of body)))
      [:Abs bndV body])))

(defn mk-binop
  "Given a binary operator and two arguments, returns the application of the operator to those arguments. Returns nil if the types are incompatible."
  [op x y]
  (mk-comb (mk-comb op x) y))

(defn dest-binop
  "Given an application of an operator to two terms, returns the operator and two arguments."
  [tm]
  (if-let [[lrator rand] (dest-comb tm)]
    (if-let [[op land] (dest-comb lrator)]
      [op land rand])))

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

(defn- mk-theorem
  [asms concl]
  [:Theorem asms concl])

(defn dest-theorem
  "Returns [assumptions concl] of a given theorem."
  [thm]
  (let [[tag assms concl] thm]
    (when (= tag :Theorem)
      [assms concl])))

(defn REFL
  "Given x, returns ⊦ x = x"
  [x]
  (mk-theorem #{} (mk-eq x x)))

(defn MK-COMB
  "Given ⊦ f = g and ⊦ x = y, returns ⊦ f x = g y"
  [fg xy]
  (let [[fgasms fgconcl] (dest-theorem fg)]
    (let [[xyasms xyconcl] (dest-theorem xy)]
      (if-let [[f g] (dest-eq fgconcl)]
        (if-let [[x y] (dest-eq xyconcl)]
          (mk-theorem (s/union xyasms fgasms)
                        (mk-eq (mk-comb f x)
                               (mk-comb g y))))))))