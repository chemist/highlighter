module Text.Highlighter.Lexers.Clojure (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Clojure"
    , lAliases = ["clojure", "clj"]
    , lExtensions = [".clj"]
    , lMimetypes = ["text/x-clojure", "application/x-clojure"]
    , lStart = root'
    , lFlags = [multiline]
    }

root' :: TokenMatcher
root' =
    [ tok ";.*$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "\\s+" (Arbitrary "Text")
    , tok "-?\\d+\\.\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "-?\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String")
    , tok "'[a-zA-Z0-9!$%&*+,/:<=>?@^_\126-]+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Symbol")
    , tok "\\\\([()/'\\\".'_!\195\130\194\167$%& ?;=#+-]{1}|[a-zA-Z0-9]+)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Char")
    , tok "(#t|#f)" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "('|#|`|,@|,|\\.)" (Arbitrary "Operator")
    , tok "(fn |def |defn |defmacro |defmethod |defmulti |defn\\- |defstruct |if |cond |let |for )" (Arbitrary "Keyword")
    , tok "(?<='\\()[a-zA-Z0-9!$%&*+,/:<=>?@^_\126-]+" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "(?<=#\\()[a-zA-Z0-9!$%&*+,/:<=>?@^_\126-]+" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "(?<=\\()(\\. |\\.\\. |\\* |\\+ |\\- |\\-\\> |\\.\\. |\\/ |\\< |\\<\\= |\\= |\\=\\= |\\> |\\>\\= |accessor |agent |agent\\-errors |aget |alength |all\\-ns |alter |and |append\\-child |apply |array\\-map |aset |aset\\-boolean |aset\\-byte |aset\\-char |aset\\-double |aset\\-float |aset\\-int |aset\\-long |aset\\-short |assert |assoc |await |await\\-for |bean |binding |bit\\-and |bit\\-not |bit\\-or |bit\\-shift\\-left |bit\\-shift\\-right |bit\\-xor |boolean |branch\\? |butlast |byte |cast |char |children |class |clear\\-agent\\-errors |comment |commute |comp |comparator |complement |concat |conj |cons |constantly |construct\\-proxy |contains\\? |count |create\\-ns |create\\-struct |cycle |dec |deref |difference |disj |dissoc |distinct |doall |doc |dorun |doseq |dosync |dotimes |doto |double |down |drop |drop\\-while |edit |end\\? |ensure |eval |every\\? |false\\? |ffirst |file\\-seq |filter |find |find\\-doc |find\\-ns |find\\-var |first |float |flush |fnseq |frest |gensym |get |get\\-proxy\\-class |hash\\-map |hash\\-set |identical\\? |identity |if\\-let |import |in\\-ns |inc |index |insert\\-child |insert\\-left |insert\\-right |inspect\\-table |inspect\\-tree |instance\\? |int |interleave |intersection |into |into\\-array |iterate |join |key |keys |keyword |keyword\\? |last |lazy\\-cat |lazy\\-cons |left |lefts |line\\-seq |list |list\\* |load |load\\-file |locking |long |loop |macroexpand |macroexpand\\-1 |make\\-array |make\\-node |map |map\\-invert |map\\? |mapcat |max |max\\-key |memfn |merge |merge\\-with |meta |min |min\\-key |name |namespace |neg\\? |new |newline |next |nil\\? |node |not |not\\-any\\? |not\\-every\\? |not\\= |ns\\-imports |ns\\-interns |ns\\-map |ns\\-name |ns\\-publics |ns\\-refers |ns\\-resolve |ns\\-unmap |nth |nthrest |or |parse |partial |path |peek |pop |pos\\? |pr |pr\\-str |print |print\\-str |println |println\\-str |prn |prn\\-str |project |proxy |proxy\\-mappings |quot |rand |rand\\-int |range |re\\-find |re\\-groups |re\\-matcher |re\\-matches |re\\-pattern |re\\-seq |read |read\\-line |reduce |ref |ref\\-set |refer |rem |remove |remove\\-method |remove\\-ns |rename |rename\\-keys |repeat |replace |replicate |resolve |rest |resultset\\-seq |reverse |rfirst |right |rights |root |rrest |rseq |second |select |select\\-keys |send |send\\-off |seq |seq\\-zip |seq\\? |set |short |slurp |some |sort |sort\\-by |sorted\\-map |sorted\\-map\\-by |sorted\\-set |special\\-symbol\\? |split\\-at |split\\-with |str |string\\? |struct |struct\\-map |subs |subvec |symbol |symbol\\? |sync |take |take\\-nth |take\\-while |test |time |to\\-array |to\\-array\\-2d |tree\\-seq |true\\? |union |up |update\\-proxy |val |vals |var\\-get |var\\-set |var\\? |vector |vector\\-zip |vector\\? |when |when\\-first |when\\-let |when\\-not |with\\-local\\-vars |with\\-meta |with\\-open |with\\-out\\-str |xml\\-seq |xml\\-zip |zero\\? |zipmap |zipper )" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "(?<=\\()[a-zA-Z0-9!$%&*+,/:<=>?@^_\126-]+" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "[a-zA-Z0-9!$%&*+,/:<=>?@^_\126-]+" (Arbitrary "Name" :. Arbitrary "Variable")
    , tok "(\\[|\\])" (Arbitrary "Punctuation")
    , tok "(\\{|\\})" (Arbitrary "Punctuation")
    , tok "(\\(|\\))" (Arbitrary "Punctuation")
    ]

