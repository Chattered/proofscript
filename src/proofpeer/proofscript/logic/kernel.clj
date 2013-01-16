(ns proofpeer.proofscript.logic.kernel
  (:require clojure.test))
(use '[clojure.test :only [is]])

;; This is not idiomatic clojure. Since this is a kernel, we use
;; sealed abstractions for everything other than atoms. Atoms for
;; terms and types are drawn from an arbitrary abstract alphabet which
;; only needs to support equality.

;; Typed lambda calculus.
(defn mk-var
  "A typed variable."
  [atom ty]
  [:var atom ty])

(defn mk-const
  "An typed constant."
  [atom ty]
  [:const atom ty])

(defn mk-app
  "A combination."
  [f x]
  [:comb f x])

(defn mk-abs
  "An abstraction."
  [x bod]
  [:abs x bod])

(defn mk-tyvar
  "A type variable."
  [atom]
  [:tyv atom])

(defn mk-tyconstant
  "A type constant."
  [atom]
  [:tyc atom])

(defn mk-tyconstructor
  "A constructed type."
  [atom args]
  [:tycons atom args])

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
    [(term 1) (term 2)]))

(defn dest-abs
  "Returns an abstraction's variable and body or nil if the term is not an abstraction."
  [term]
  (when (= (first term) :abs)
    [(term 1) (term 2)]))

(defn dest-tyconstructor
  "Returns a type's constructor and arguments in a vector, or nil if the type is not a constructed type."
  [ty]
  (when (= (first ty) :tycons)
    (next ty)))

(let [my-tyv   (mk-tyvar         'x)
      my-tyc   (mk-tyconstant    'X)
      my-tyfun (mk-tyconstructor '->     [my-tyv my-tyv])
      my-var   (mk-var           0       my-tyv)
      my-const (mk-const         0       my-tyv)
      my-abs   (mk-abs           my-var  my-var)
      my-comb  (mk-app           my-abs  my-var)]
  (do 
    (is (= (dest-var   my-var)   0))
    (is (= (dest-var   my-const) nil))
    (is (= (dest-var   my-comb)  nil))
    (is (= (dest-var   my-abs)   nil))
    (is (= (dest-const my-var)   nil))
    (is (= (dest-const my-const) 0))  
    (is (= (dest-const my-comb)  nil))
    (is (= (dest-const my-abs)   nil))
    (is (= (dest-comb  my-var)   nil))
    (is (= (dest-comb  my-const) nil))  
    (is (= (dest-comb  my-comb)  [my-abs my-var]))
    (is (= (dest-comb  my-abs)   nil))
    (is (= (dest-abs   my-var)   nil))
    (is (= (dest-abs   my-const) nil))  
    (is (= (dest-abs   my-comb)  nil))
    (is (= (dest-abs   my-abs)   [my-var my-var]))))

(defn fun-ty [fty xty]
  (mk-tyconstructor :-> [fty xty]))

(defn dest-fun-ty
  "Returns the rator and rand types in a vector, or nil if the type is not a function type."
  [ty]
  (if-let [tyargs (dest-tyconstructor ty)]
    [(first tyargs)
     (tyargs 1)]))