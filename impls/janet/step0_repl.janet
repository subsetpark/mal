(defn READ [x] x)
(defn EVAL [x] x)
(defn PRINT [x] x)

(defn rep [x]
  (-> x
     (READ)
     (EVAL)
     (PRINT)))

(forever
 (prin "user> ")
 (let [input (getline)]
   (cond 
     (= (length input) 0) (os/exit 0)
     true (-> input
             (rep)
             (print)))))

