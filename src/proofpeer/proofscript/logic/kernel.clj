(ns proofpeer.proofscript.logic.kernel
  (:require clojure.test))
(use '[clojure.test :only [is]])

;; This is not idiomatic clojure. Since this is a kernel, we use
;; sealed abstractions for everything other than atoms. Atoms for
;; terms and types are drawn from an arbitrary abstract alphabet which
;; only needs to support equality.

;; Typed lambda calculus.
(defn mk-var
  "Returns a typed variable."
  [atom ty]
  [:var atom ty])

(defn mk-const
  "Returns a typed constant."
  [atom ty]
  [:const atom ty])

(defn mk-app
  "Returns a combination."
  [f x]
  [:comb f x])

(defn mk-abs
  "Returns an abstraction."
  [x bod]
  [:abs x bod])

(defn mk-tyvar
  "Returns a type variable."
  [atom]
  [:tyv atom])

(defn mk-tyconstant
  "Returns a type constant."
  [atom]
  [:tyc atom])

(defn mk-tyconstructor
  "Returns a constructed type."
  [atom & args]
  (cons :tycons (cons atom args)))

(defn dest-var
  "Returns a variable's atom or nil if the term is not a variable."
  [term]
  (if (= (first term) :var)
    (term 1)))

(defn dest-const
  "Returns a constant's atom or nil if the term is not a constant."
  [term]
  (when (= (first term) :const)
    (term 1)))

(defn dest-comb
  "Returns a combination's rator and rand or nil if the term is not a combination."
  [term]
  (when (= (first term) :comb)
    (rest term)))

(defn dest-abs
  "Returns an abstraction's variable and body or nil if the term is not an abstraction."
  [term]
  (when (= (first term) :abs)
    (rest term)))

(defn dest-tyconstructor
  "Returns a type's constructor and arguments in a vector, or nil if the type is not a constructed type."
  [ty]
  (when (= (first ty) :tycons)
    (rest ty)))

(defn mk-fun-ty [tyx tyy]
  (mk-tyconstructor :-> tyx tyy))

(defn dest-fun-ty
  "Returns the rator and rand types in a vector, or nil if the type is not a function type."
  [ty]
  (if-let [[_ & tyargs] (dest-tyconstructor ty)]
    tyargs))

(defn type-of
  "Returns the type of the given term. If the term cannot be consistently typed, returns nil."
  [term]
  (case (first term)
    :var    (term 2)
    :const  (term 2)
    :comb   (let [[f     x] (rest term)
                  [tyx tyy] (dest-fun-ty (type-of f))]
              (when (= tyx (type-of x))
                tyy))
    :abs    (let [[x   bod] (rest term)
                  tyx       (type-of x)
                  tyy       (type-of bod)]
              (mk-fun-ty tyx tyy))))