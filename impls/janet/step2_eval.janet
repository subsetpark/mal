(import /reader)
(import /printer)

(varfn EVAL 'tk)

(defn eval-ast [ast env]
  (match ast
    (s (symbol? s)) (match (in env s)
                      nil (error [:notfound ast])
                      evaled evaled)
    (@[type seq] (indexed? seq)) [type (map |(EVAL $ env) seq)]
    else else))

(defn- list? [ast]
  (match ast
    (@[:parens seq] (indexed? seq)) true
    false))

(defn READ [x] (reader/read-str x))

(varfn EVAL [ast env]
  (match ast
    (@[:parens seq] (empty? seq)) ast
    (@[:parens seq] (indexed? seq)) (let [[_ evaled] (eval-ast ast env)]
                                      (apply (first evaled) (array/slice evaled 1)))
    _ (eval-ast ast env)))

(defn PRINT [x] (printer/print-str x))

(def- base-env
  {'+ +
   '- -
   '* *
   '/ (comp math/floor /)})

(defn rep [x]
  (try
    (-> x
        (READ)
        (EVAL base-env)
        (PRINT))
    ([err] (match err
             [:unexpected-eof delim] (string/format "unbalanced delimiter: expected %q" delim)
             :endquote "unbalanced quotation marks"
             [:mismatch expected got] (string/format "delimiter mismatch: got %q, expected %q"
                                                     got
                                                     expected)
             [:notfound sym] (string/format "symbol not found: %q"
                                            sym)
             other (string/format "unknown error: %q" other)))))

(forever
  (:write (dyn :out stdout) "user> ")
  (flush)
  (let [input (getline)]
    (cond
      (= (length input) 0) (os/exit 0)
      true (-> input
               (rep)
               (print)))))
