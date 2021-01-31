(import /reader)
(import /printer)

(defn READ [x] (reader/read-str x))
(defn EVAL [x] x)
(defn PRINT [x] (printer/print-str x))

(defn rep [x]
  (-> x
      (READ)
      (EVAL)
      (PRINT)))

(forever
  (:write (dyn :out stdout) "user> ")
  (flush)
  (let [input (getline)]
    (cond
      (= (length input) 0) (os/exit 0)
      true (-> input
               (rep)
               (print)))))
