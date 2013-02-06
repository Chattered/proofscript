;;;; To ensure logical soundness, only access the data-structures in this
;;;; kernel via the exported functions.

(ns proofpeer.proofscript.logic.kernel
  "A kernel for the simply typed lambda calculus together with ZFC set
theory. Formally: we allow terms to be rank-1 polymorphic and we allow
type-constructors provided that all their type arguments havke kind *.

Additionally, all types are indexed by a context, which is used to associate
axioms and definitions with types, terms and theorems. The context is an
arbitrary value such that types with distinct context are taken to be
distinct. The nil context is reserved for the kernel.

Functions with doc-strings introduced with CONTEXT are provided for clients
providing implementations of contexts.  "
  (:require clojure.test))
(use '[clojure.test :only [is]])

;; The user can freely move types, terms and theorems between contexts, but it
;; is expected that restrictions on this will be imposed from outside the
;; kernel. For instance, it might be a good idea to follow HOL Light and
;; disallow type constructors with the same name and disallow overloaded
;; constants.

(defrecord TypeConstr [cons context args])
(defrecord TypeVar    [symbol])
(defrecord Const      [symbol type])
(defrecord Var        [symbol type])
(defrecord Comb       [rator rand])
(defrecord Abs        [var body])

(defn- context-of-ty
  [ty]
  (condp (type ty)
      TypeConstr (.context ty)
      TypeVar    nil))

(defn mk-ty-constr
  "Apply a type constructor in a context. All type arguments must either belong
  to the root context -- nil -- or belong to the given same context.

  Returns nil if the contexts do not match."
  [cons context & args]
  (when (every? #(or (not %) (= % context)) args)
    (-> TypeConstr cons context args)))

(defn mk-ty-var
  "Create a type variable."
  [sym]
  (-> TypeVar sym))

(defn fun-ty
  "Apply the function type constructor in the root context."
  [ty-rator ty-rand]
  (mk-ty-constr '-> nil ty-rator ty-rand))

(defn type-of
  [tm]
  (condp (type tm)
      Const (.type tm)
      Var   (.type tm)
      Comb  (-> tm .rator type-of 1)
      Abs   (fun-ty
             (-> tm .var type-of)
             (-> tm .body type-of))))

(defn mk-const
  "CONTEXT: Construct a typed constant. Context implementations might want to
prevent overloading before calling this function."
  [sym type]
  (-> Const sym type))

(defn mk-var
  "Construct a typed variable."
  [sym type]
  (-> Var sym type))

;; (defn mk-ty-var
;;   "Create a type variable."
;;   [sym]
;;   (-> TypeVar sym))

;; (defn mk-const
;;   "CONTEXT: Create a typed constant. Context implementations might want to
;; prevent overloading here."
;;   [sym type]
;;   (-> Const sym type))

;; (defn mk-var
;;   "Create a typed variable."
;;   [sym type]
;;   (-> Var sym type))

;; (defn- context-of-ty
;;   "The context of a type."
  

;; (defn context-of
;;   "The context of a term."
;;   [term]
;;   (condp = (type term)
;;     Const (.type term)

;; (def mk-fun-ty
;;   "Contextualised function types."
;;   [context rator-ty rand-ty]
;;   (TypeConst '-> (or (context-of rator-ty) (context-of rand-ty))))


;; (defn- tyctx-of
;;   "Determine the contextualised type of a term."
;;   [term]
;;   (condp = (type term)
;;     Const (.type term)
;;     Var   (.type var)
;;     App   (-> rator .type .args 2)
;;     Abs   (mk-fun-ty (context-of

;; (defn mk-app
;;   "Creates an application from a rator and rand. If not well-typed, returns
;; nil."
;;   [rator rand]
;;   (

;; (defn context-of-type [ty]
;;   (condp = (type ty)
;;     ConstType (.context ty)
;;     VarType   nil))

;; (defn context-of-atom
;;   "Returns the context of a constant or variable."
;;   [atom]
;;   (context-of-type (.type const)))

;; (defn context-of-app
;;   "Returns the context of an application."
;;   [app]
;;   (-> app .rator)

;;   (defn mk-var
;;   "Returns a typed variable."
;;   [atom ty]
;;   [:var atom ty])

;; (defn mk-const
;;   "Returns a typed constant."
;;   [atom ty]
;;   [:const atom ty])

;; (defn mk-app
;;   "Returns a combination."
;;   [f x]
;;   [:comb f x])

;; (defn mk-abs
;;   "Returns an abstraction."
;;   [x bod]
;;   [:abs x bod])

;; (defn mk-tyvar
;;   "Returns a type variable."
;;   [atom]
;;   [:tyv atom])

;; (defn mk-tyconstant
;;   "Returns a type constant."
;;   [atom]
;;   [:tyc atom])

;; (defn mk-tyconstructor
;;   "Returns a constructed type."
;;   [atom & args]
;;   (cons :tycons (cons atom args)))

;; (defn dest-var
;;   "Returns a variable's atom or nil if the term is not a variable."
;;   [term]
;;   (if (= (first term) :var)
;;     (term 1)))

;; (defn dest-const
;;   "Returns a constant's atom or nil if the term is not a constant."
;;   [term]
;;   (when (= (first term) :const)
;;     (term 1)))

;; (defn dest-comb
;;   "Returns a combination's rator and rand or nil if the term is not a combination."
;;   [term]
;;   (when (= (first term) :comb)
;;     (rest term)))

;; (defn dest-abs
;;   "Returns an abstraction's variable and body or nil if the term is not an abstraction."
;;   [term]
;;   (when (= (first term) :abs)
;;     (rest term)))

;; (defn dest-tyconstructor
;;   "Returns a type's constructor and arguments in a vector, or nil if the type is not a constructed type."
;;   [ty]
;;   (when (= (first ty) :tycons)
;;     (rest ty)))

;; (defn mk-fun-ty [tyx tyy]
;;   (mk-tyconstructor :-> tyx tyy))

;; (defn dest-fun-ty
;;   "Returns the rator and rand types in a vector, or nil if the type is not a function type."
;;   [ty]
;;   (if-let [[_ & tyargs] (dest-tyconstructor ty)]
;;     tyargs))

;; (defn type-of
;;   "Returns the type of the given term. If the term cannot be consistently typed, returns nil."
;;   [term]
;;   (case (first term)
;;     :var    (term 2)
;;     :const  (term 2)
;;     :comb   (let [[f     x] (rest term)
;;                   [tyx tyy] (dest-fun-ty (type-of f))]
;;               (when (= tyx (type-of x))
;;                 tyy))
;;     :abs    (let [[x   bod] (rest term)
;;                   tyx       (type-of x)
;;                   tyy       (type-of bod)]
;;               (mk-fun-ty tyx tyy))))

