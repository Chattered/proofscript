(ns proofpeer.logic.kernel)

;; Types
(defrecord Type [kind param])

(def const-fun "Name of type constructor for functions" {:theory [] :name "fun"})
(def const-bool "Name of type constructor for booleans / propositions." {:theory [] :name "bool"})

(def ty-set "The type of Zermelo-Fraenkel sets."  (->Type ::ty-set nil))
(defn ty-app "The type of applications of name to args.
              The parameter name must correspond to a type constructor."
  [name args] (->Type ::ty-app {:name name :args args}))
(defn ty-var "Type variable." [varname] (->Type ::ty-var varname))

(defn ty-fun "Type of functions from u to v." [u v] (ty-app const-fun [u v]))
(def ty-bool "Type of booleans / propositions." (ty-app const-bool []))

(defn ty-app?
  "If only ty is given, returns a map {:name name :args args}
     if this is a type application, otherwise nil.
   If both ty and name are passed as parameters, returns the
     sequence of arguments if this is a type application, otherwise nil."
  ([ty]
     (if (= (:kind ty) ::ty-app)
       (:param ty)))
  ([ty name]
     (if-let [p (ty-app? ty)]
       (if (= (:name p) name) (:args p)))))

(defn ty-fun? [ty] (ty-app? ty const-fun))
(defn ty-bool? [ty] (if (ty-app? ty const-bool) true))
(defn ty-set? [ty] (= (:kind ty) ::ty-set))
(defn ty-var? [ty] (if (= (:kind ty) ::ty-var) (:param ty)))

;; Terms
(defrecord Term [kind param])

(defn tm-const
  "Term constant of given name and type.
   If the optional mapped is true (default is false), then this
   constant is to be understood not in the sense it was originally defined,
   but in the mapped sense."
  ([name type] (tm-const name type nil))
  ([name type mapped] (->Term ::tm-const {:name name :type type :mapped (if mapped true false)})))
(defn tm-bound "Local variable, specified via de bruijn index." [index] (->Term ::tm-bound index))
(defn tm-app "Application of f to g" [f g] (->Term ::tm-app {:fun f :arg g}))
(defn tm-abs "Lambda abstraction, body abstracts over variable of given name and type."
  [name type body] (->Term ::tm-abs {:name name :type type :body body}))

(defn- tm-destr [k tm] (if (= (:kind tm) k) (:param tm)))
(defn tm-const? [tm] (tm-destr ::tm-const tm))
(defn tm-bound? [tm] (tm-destr ::tm-bound tm))
(defn tm-app? [tm] (tm-destr ::tm-app tm))
(defn tm-abs? [tm] (tm-destr ::tm-abs tm))

;; Contexts
(defprotocol Context
  "Types and terms always live in a context. The context determines what constants
   there are and what type they have."
  (type-of-const [this name]
    "Returns the type of the constant as it was originally defined.
     If there is no constant with this name, returns nil.")
  (mapped-types-of-const [this name]
    "Returns a sequence of the mapped types that have been registered for this constant.
     If there is no constant with this name, returns nil.")
  (type-constructor? [this name]
    "Returns nil if name does not denote a term constant that has been registered as a type constructor.
     Otherwise returns the arity of the resulting type constructor."
    [this name]))

;; Checking validity of types and terms. 
(declare valid-term?)
(declare valid-type?)

(defn valid-type? "Checks if ty is a valid type in the given context."
  [context ty]
  (let [k (:kind ty)
        p (:param ty)]
    (cond
      (= k ::ty-set) true
      (= k ::ty-var) true
      (= k ::ty-app)
      (let [name (:name p)
            args (:args p)
            arity (type-constructor? context name)]
        (if (and arity (= (count args) arity))         
          (every? #(valid-type? context %) args)
          false))
      :else false)))

(defn set-like?
  "Checks if ty is a type that, if valid, can be interpreted as a Zermelo-Fraenkel set."
  [ty]
  (let [k (:kind ty)
        p (:param ty)]
    (cond
      (= k ::ty-set) false
      (= k ::ty-var) true
      (= k ::ty-app) (every? set-like? (:args p))
      :else false)))

(defn ty-collect-tyvars
  "Adds (via conj) all polymorphic type variable names in ty to vars."
  ([vars ty]
     (let [k (:kind ty)
           p (:param ty)]
       (cond
         (= k ::ty-set) vars
         (= k ::ty-var) (conj vars p)
         (= k ::ty-app) (reduce ty-collect-tyvars vars (:args p))
         :else vars)))
  ([ty] (ty-collect-tyvars #{} ty)))

(defn ty-polymorphic?
  "Checks if ty is polymorphic, i.e. contains type variables."
  [ty] (not (empty? (ty-collect-tyvars ty))))

(defn type-constructor-arity
  "Returns the arity of ty if ty can be interpreted as type constructor,
   i.e. is an n-ary function, n >= 0, from sets to set.
   Returns nil if this type cannot be interpreted as type constructor."
  [ty]
  (if (ty-set? ty)
    0
    (if-let [[u v] (ty-fun? ty)]
      (if (ty-set? u)
        (if-let [a (type-constructor-arity v)]
          (+ a 1))))))

(defn ty-match-with
  [ty-instance ty S]
  (cond
    (ty-var? ty) (let [v (ty-var? ty)]
                   (if-let [vty (S v)]
                     (if (= vty ty-instance) S)
                     (assoc S v ty-instance)))
    (ty-set? ty) (if (ty-set? ty-instance) S)
    (ty-app? ty) (let [{name :name args :args} (ty-app? ty)]
                   (if-let [{i-name :name i-args :args} (ty-app? ty-instance)]
                     (if (and (= name i-name) (= (count args) (count i-args)))
                       (reduce #(if %1 (let [[ity ty] %2] (ty-match-with ity ty %1)))
                               S (partition 2 (interleave i-args args))))))))

(defn ty-match
  "Calculates the substitution S such that S applied to ty yields ty-instance.
   Returns nil if there is no such subsitution."
  [ty-instance ty] (ty-match-with ty-instance ty {}))

(defn ty-instanceof?
  "Checks if the type ty-instance is an instance of ty."
  [ty-instance ty]
  (if (ty-match ty-instance ty) true))

(defn valid-const?
  "Checks if (tm-const name ty mapped) is a valid constant of valid type ty in the given context.
   If so, returns ty, otherwise returns nil."
  [context {name :name ty :type mapped :mapped}]
  (if (valid-type? context ty)
    (if (not mapped)
      (if-let [oty (type-of-const context name)]
        (if (ty-instanceof? ty oty) ty)
      (if-let [tys (mapped-types-of-const context name)]
        (if (some #(ty-instanceof? ty %) tys) ty))))))

(defn valid-term?
  "Check if tm is a valid term in the given context.
   If so, returns the type of the term, otherwise false."
  ([context tm] (valid-term? context tm '()))
  ([context tm vars]
     (cond
       (tm-const? tm) (valid-const? context (tm-const? tm))
       (tm-bound? tm) (nth vars (tm-bound? tm) nil)
       (tm-app? tm) (let [{f :fun x :arg} (tm-app? tm)]
                      (if-let [fty (valid-term? context f vars)]
                        (if-let [xty (valid-term? context x vars)]
                          (if-let [[u v] (ty-fun? fty)]
                            (if (= u xty) v)))))
       (tm-abs? tm) (let [{ty :type body :body} (tm-abs? tm)]
                      (if (valid-type? context ty)
                        (if-let [bty (valid-term? context body (cons ty vars))]
                          (ty-fun ty bty)))))))







