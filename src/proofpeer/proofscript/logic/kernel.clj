;;;; To ensure logical soundness, only access the data-structures in this
;;;; kernel via the exported functions.

(ns proofpeer.proofscript.logic.kernel
  "A kernel for the simply typed lambda calculus together with ZFC set
theory. Formally: we allow terms to be rank-1 polymorphic and we allow
type-constructors provided that all their type arguments havke kind *.

Additionally, all types are indexed by a context, which is used to associate
axioms and definitions with types, terms and theorems. The context is an
arbitrary value such that types with distinct context are taken to be
distinct.

The nil context is reserved for the most logically secure parts of the
kernel. The only type-constructors in the nil context are:
  1) the nullary constructor for ZF sets
  2) the polymorphic function type constructor
The only axioms and inference rules are those for HOL + ZFC. The kernel does
not permit any new axioms in the nil context.

New axioms can be added into any other context according to whatever
conventions are adopted by the context implementation. For instance, a context
may wish to disallow overloaded constants and variadic type constructors.

Functions with doc-strings introduced with CONTEXT are provided for clients
providing implementations of contexts."
  )

;;; Syntax
(defrecord TypeConstr [cons context args])
(defrecord TypeVar    [symbol])
(defrecord Var        [symbol type])
(defrecord Comb       [rator rand])
(defrecord Abs        [bndV body])

(defn- context-of-ty
  [ty]
  (condp = (class ty)
      TypeConstr (.context ty)
      TypeVar    nil))

(defn context-of-term
  "Returns the context of a given term."
  [term]
  (condp = (type term)
      Var   (context-of-ty   (.type  term))
      Comb  (context-of-term (.rator term))
      Abs   (context-of-term (.bndV  term))))

(defn mk-ty-constr
  "Apply a type constructor in a context. All type arguments must either belong
  to the root context -- nil -- or belong to the given context.

  Returns nil if the contexts do not match."
  [cons context & args]
  (when (every? #(or (not %) (= (context-of-ty %) context)) args)
    (->TypeConstr cons context args)))

(defn mk-ty-var
  "Create a type variable."
  [sym]
  (->TypeVar sym))

(defn mk-fun-ty
  "Apply the function type constructor in the root context."
  [ty-dom ty-codom]
  (mk-ty-constr '-> nil ty-dom ty-codom))

(defn dest-fun-ty
  "Returns the rator and rand type of a function type in a vector, or nil if
the argument is not a function type."
  [ty]
  (when (and (= (type ty) TypeConstr)
             (= (.cons ty) '->))
    (.args ty)))
  
(defn type-of
  [tm]
  (condp = (type tm)
      Var   (.type tm)
      Comb  (-> tm .rator type-of 1)
      Abs   (mk-fun-ty
             (-> tm .var type-of)
             (-> tm .body type-of))))

(defn mk-var
  "Construct a typed variable."
  [sym type]
  (->Var sym type))

(defn mk-comb
  "Construct a typed combination. Returns nil if the rator and rand types do not agree."
  [rator rand]
  (if-let [[dom-ty _] (dest-fun-ty (type-of rator))]
    (when (= dom-ty (type-of rand))
      (->Comb rator rand))))

(defn mk-abs
  "Construct a typed abstraction. Returns nil if the variable is not in the
same context as the body."
  [bndV body]
  (when (= (-> bndV .type .context) (context-of-term body))
    (->Abs bndV body)))

(defn mk-binop
  "Given a binary operator and two arguments, returns the term denoting the image of a operation applied to those arguments."
  [op x y]
  (mk-comb (mk-comb op (mk-comb x)) y))

(def bool-ty
  "The type of truth values."
  (mk-ty-var 'bool))

(def foo
   (mk-fun-ty (mk-ty-var 1) (mk-ty-var 2)))

;; (def eq
;;   "Equality."
;;   (let [eq-ty (mk-fun-ty (->TypeVar 3)
;;                          (mk-fun-ty :bool (->TypeVar 'α)))]
;;     (mk-var '= eq-ty)))

;; (defn mk-eq
;;   "Given x and y, returns the term x = y."
;;   [x y]
;;   (mk-binop 

(defrecord Theorem [assumptions concl])

;; (defn refl [x]
;;   "Given x, returns |- x = x"
;;   [x]
;;   (mk-
