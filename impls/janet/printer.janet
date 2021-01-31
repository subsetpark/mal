(varfn print-str [ds] 'tk)

(defn- join [left right seq]
  (string left (string/join (map print-str seq) " ") right))

(varfn print-str [ds]
  (match ds
    @[:brackets seq] (join "[" "]" seq)
    @[:curly seq] (join "{" "}" seq)
    @[:parens seq] (join "(" ")" seq)
    sym (string sym)))
