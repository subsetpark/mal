#
# Reader
#

(def Reader @{:_get (fn [self pos] (try (in (self :tokens) pos)
                                     ([err] :eof)))
              :next (fn [self]
                      (let [pos (self :position)]
                        (++ (self :position))
                        (:_get self pos)))
              :peek (fn [self] (:_get self (self :position)))})

(defn new-reader [tokens] (table/setproto @{:position 0 :tokens tokens} Reader))

#
# Tokenizer
#

# PCRE: [\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)
(def _peg ~{:whitespace (any (+ "," :s))
            :delimiter (set "[]{}()'`~^@")
            :string (* "\"" (any (+ (* "\\" 1) (if-not "\"" 1))) (opt "\""))
            :comment (* ";" (any (if-not "\n" 1)))
            :symbol (* (some (if-not (+ :s (set "[]{}('\"`,;)")) 1)))
            :main (some (* :whitespace
                           (<- (+ "~@" :delimiter :string :comment :symbol))))})
(def peg (peg/compile _peg))

(defn tokenize [str] (peg/match peg str))

#
# Forward Declarations
#

(varfn read-list [reader] 'tk)
(varfn read-form [reader] 'tk)

#
# Reader Macros
#

(defmacro- defreader [special-form forms exp]
  ~(defn- ,(symbol "read-" special-form)
     [reader]
     (let ,(mapcat |[$ '(read-form reader)] forms)
       ,exp)))

(defreader deref [form] [:parens ['deref form]])
(defreader quote [form] [:parens ['quote form]])
(defreader quasiquote [form] [:parens ['quasiquote form]])
(defreader unquote [form] [:parens ['unquote form]])
(defreader splice-unquote [form] [:parens ['splice-unquote form]])
(defreader with-meta [meta form] [:parens ['with-meta form meta]])

#
# Reading
#

(def- quote-byte (chr "\""))
(def- colon-byte (chr ":"))

(defn read-atom [reader]
  (let [tok (:next reader)]
    (match tok
      ")" :rp
      "]" :rb
      "}" :rc
      (str (= quote-byte (first str)) (not= quote-byte (last str))) (error :endquote)
      (str (= quote-byte (first str))) tok
      (str (= colon-byte (first str))) (keyword (string/slice tok 1))
      _ (match (scan-number tok)
          nil (symbol tok)
          num num))))


(defmacro- advance-and [& body]
  ~(do (:next reader) ,body))

(varfn read-form [reader]
  (case (:peek reader)
    :eof :eof
    "(" (advance-and read-list reader :rp)
    "[" (advance-and read-list reader :rb)
    "{" (advance-and read-list reader :rc)
    "@" (advance-and read-deref reader)
    "'" (advance-and read-quote reader)
    "`" (advance-and read-quasiquote reader)
    "~" (advance-and read-unquote reader)
    "~@" (advance-and read-splice-unquote reader)
    "^" (advance-and read-with-meta reader)

    (read-atom reader)))

(varfn read-list [reader end]
  (def end->type {:rp :parens :rb :brackets :rc :curly})
  (defn do-read-list [reader acc]
    (match (read-form reader)
      :eof (error [:eof end])
      (sym (= end sym)) [(end->type end) acc]
      (sym (or (= :rp sym) (= :rc sym) (= :rb sym))) (error [:mismatch end sym])
      form (do-read-list reader (array/push acc form))))
  (do-read-list reader @[]))

(defn read-str [str]
  (-> str
      (tokenize)
      (new-reader)
      (read-form)))
