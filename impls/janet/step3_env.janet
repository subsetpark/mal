(import /reader)
(import /printer)
(import /env)

(varfn EVAL 'tk)

(defn eval-ast [ast env]
  (match ast
    (s (symbol? s)) (:get! env s)
    (@[type seq] (indexed? seq)) [type (map |(EVAL $ env) seq)]
    else else))

(defn apply-form [[type ast] env]
  (match ast
    (@[sym key val] (= sym 'def!)) (let [evaled (EVAL val env)]
                        (put env key evaled)
                        evaled)
    (@[sym @[_type bindings] body] (= sym 'let*)) (let [new-env (env/new-env env)
                                  binding-pairs (partition 2 bindings)]
                              (loop [[k v] :in binding-pairs]
                                (put new-env k (EVAL v new-env)))
                              (EVAL body new-env))
    _ (let [[_ evaled] (eval-ast [type ast] env)]
        (apply (first evaled) (array/slice evaled 1)))))

(defn READ [x] (reader/read-str x))

(varfn EVAL [ast env]
  (match ast
    (@[:parens seq] (empty? seq)) ast
    (@[:parens seq] (indexed? seq)) (apply-form ast env)
    _ (eval-ast ast env)))

(defn PRINT [x] (printer/print-str x))

(def- base-env
  (env/new-env env/Env
               '+ +
               '- -
               '* *
               '/ (comp math/floor /)))

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
             [:notfound sym] (string/format "%q not found"
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
