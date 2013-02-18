(ns proofpeer.proofscript.logic.kernel
  "The proof peer kernel. To guarantee logical soundness, all data should be
  constructed from the exported functions.

   The basic syntax of the kernel supports type variables, type constructors,
variables, constants, combinations and abstractions. However, the arguments to
all type constructors must have kind *. Additionally, all types are indexed by
a context, which is used to associate axioms and definitions with types, terms
and theorems. The context is an arbitrary value such that types with distinct
context are taken to be distinct.

All contexts are greater than the nil context and otherwise incomparable. See
the documentation for mk-ty-constr."

  (:use phlegmaticprogrammer.clojure_util.core)
  )

(defn context-of-ty
  [ty]
  (condp = (first ty)
      :TypeConstr (let [[_ context _] (rest ty)]
                    context)
      :TypeVar    nil))

(defn context-of-term
  "Returns the context of a given term."
  [term]
  (condp = (first term)
      :Var   (let [[_ ty] (rest term)] (context-of-ty ty))
      :Const (let [[_ ty] (rest term)] (context-of-ty ty))
      :Comb  (let [[rator _] (rest term)] (context-of-term rator))
      :Abs   (let [[bndV _] (rest term)] (context-of-term bndV))))

(defn mk-ty-constr
  "Apply a type constructor in a context. The context of the returned type is
the maximum of the given context and the contexts of all type argument
contexts. Returns nil if no such maximum exists."
  [cons context & args]
  (let [[maxContext valid]
        (reduce #(let [[maxC valid] %1
                       cx           (context-of-ty %2)] 
                   (and valid
                        (if (and maxC cx)
                          (if (= maxC cx)
                            [maxC true]
                            [nil false])
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
  "Returns the rator and rand type of a function type in a vector, or nil if
the argument is not a function type."
  [ty]
  (if-let [[tag cons context args] ty]
    (when (and (= tag :TypeConstr)
               (= cons '->))
      (if (= (count args) 2)
        args))))
  
(defn type-of
  [tm]
  (condp = (first tm)
      :Var   (let [[_ ty] (rest tm)] ty)
      :Const (let [[_ ty] (rest tm)] ty)
      :Comb  (let [[rator rand] (rest tm)] (second (dest-fun-ty (type-of rator))))
      :Abs   (let [[v bod] (rest tm)]
               (mk-fun-ty (type-of v) (type-of bod)))))

(defn mk-var
  "Construct a typed variable."
  [sym type]
  [:Var sym type])

(defn mk-const
  "Construct a typed constant."
  [sym type]
  [:Const sym type])

(defn mk-comb
  "Construct a typed combination. Returns nil if the rator and rand types do not agree."
  [rator rand]
  (if-let [[dom-ty _] (dest-fun-ty (type-of rator))]
    (when (= dom-ty (type-of rand))
      [:Comb rator rand])))

(defn dest-comb
  "Return the rator and rand of a term. Returns nil if not a combination."
  [tm]
  (when (= :Comb (first tm))
    (rest tm)))

(defn mk-abs
  "Construct a typed abstraction. Returns nil if the variable is not in the
same context as the body."
  [bndV body]
  (let [[tag sym ty] bndV]
    (if (= tag :Var)
      (when (= (context-of-ty ty) (context-of-term body))
        [:abs bndV body])
      (runtime-error "First argument to mk-abs must be a term variable."))))

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

(defn mk-theorem
  "For the inference rules only!"
  [asms concl]
  [:Theorem asms concl])