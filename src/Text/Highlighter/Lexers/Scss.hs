module Text.Highlighter.Lexers.Scss (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "SCSS"
    , lAliases = ["scss"]
    , lExtensions = [".scss"]
    , lMimetypes = ["text/x-scss"]
    , lStart = root'
    , lFlags = [caseless, dotall]
    }

stringUrl' :: TokenMatcher
stringUrl' =
    [ tok "(\\\\#|#(?=[^\\n{])|[^\\n#)])+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tokNext "#\\{" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol") (GoTo interpolation')
    , tokNext "\\)" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") Pop
    ]

attr' :: TokenMatcher
attr' =
    [ tok "[^\\s:=\"\\[]+" (Arbitrary "Name" :. Arbitrary "Attribute")
    , tokNext "#{" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol") (GoTo interpolation')
    , tokNext "[ \\t]*:" (Arbitrary "Operator") (GoTo value')
    ]

stringDouble' :: TokenMatcher
stringDouble' =
    [ tok "(\\\\.|#(?=[^\\n{])|[^\\n\"#])+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tokNext "#\\{" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol") (GoTo interpolation')
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") Pop
    ]

class' :: TokenMatcher
class' =
    [ tok "[\\w-]+" (Arbitrary "Name" :. Arbitrary "Class")
    , tokNext "#\\{" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol") (GoTo interpolation')
    , tokNext "" (Arbitrary "Text") Pop
    ]

pseudoClass' :: TokenMatcher
pseudoClass' =
    [ tok "[\\w-]+" (Arbitrary "Name" :. Arbitrary "Decorator")
    , tokNext "#\\{" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol") (GoTo interpolation')
    , tokNext "" (Arbitrary "Text") Pop
    ]

value' :: TokenMatcher
value' =
    [ tok "[ \\t]+" (Arbitrary "Text")
    , tok "[!$][\\w-]+" (Arbitrary "Name" :. Arbitrary "Variable")
    , tokNext "url\\(" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other") (GoTo stringUrl')
    , tok "[a-z_-][\\w-]*(?=\\()" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "(azimuth|background-attachment|background-color|background-image|background-position|background-repeat|background|border-bottom-color|border-bottom-style|border-bottom-width|border-left-color|border-left-style|border-left-width|border-right|border-right-color|border-right-style|border-right-width|border-top-color|border-top-style|border-top-width|border-bottom|border-collapse|border-left|border-width|border-color|border-spacing|border-style|border-top|border|caption-side|clear|clip|color|content|counter-increment|counter-reset|cue-after|cue-before|cue|cursor|direction|display|elevation|empty-cells|float|font-family|font-size|font-size-adjust|font-stretch|font-style|font-variant|font-weight|font|height|letter-spacing|line-height|list-style-type|list-style-image|list-style-position|list-style|margin-bottom|margin-left|margin-right|margin-top|margin|marker-offset|marks|max-height|max-width|min-height|min-width|opacity|orphans|outline|outline-color|outline-style|outline-width|overflow|padding-bottom|padding-left|padding-right|padding-top|padding|page|page-break-after|page-break-before|page-break-inside|pause-after|pause-before|pause|pitch|pitch-range|play-during|position|quotes|richness|right|size|speak-header|speak-numeral|speak-punctuation|speak|speech-rate|stress|table-layout|text-align|text-decoration|text-indent|text-shadow|text-transform|top|unicode-bidi|vertical-align|visibility|voice-family|volume|white-space|widows|width|word-spacing|z-index|bottom|left|above|absolute|always|armenian|aural|auto|avoid|baseline|behind|below|bidi-override|blink|block|bold|bolder|both|capitalize|center-left|center-right|center|circle|cjk-ideographic|close-quote|collapse|condensed|continuous|crop|crosshair|cross|cursive|dashed|decimal-leading-zero|decimal|default|digits|disc|dotted|double|e-resize|embed|extra-condensed|extra-expanded|expanded|fantasy|far-left|far-right|faster|fast|fixed|georgian|groove|hebrew|help|hidden|hide|higher|high|hiragana-iroha|hiragana|icon|inherit|inline-table|inline|inset|inside|invert|italic|justify|katakana-iroha|katakana|landscape|larger|large|left-side|leftwards|level|lighter|line-through|list-item|loud|lower-alpha|lower-greek|lower-roman|lowercase|ltr|lower|low|medium|message-box|middle|mix|monospace|n-resize|narrower|ne-resize|no-close-quote|no-open-quote|no-repeat|none|normal|nowrap|nw-resize|oblique|once|open-quote|outset|outside|overline|pointer|portrait|px|relative|repeat-x|repeat-y|repeat|rgb|ridge|right-side|rightwards|s-resize|sans-serif|scroll|se-resize|semi-condensed|semi-expanded|separate|serif|show|silent|slow|slower|small-caps|small-caption|smaller|soft|solid|spell-out|square|static|status-bar|super|sw-resize|table-caption|table-cell|table-column|table-column-group|table-footer-group|table-header-group|table-row|table-row-group|text|text-bottom|text-top|thick|thin|transparent|ultra-condensed|ultra-expanded|underline|upper-alpha|upper-latin|upper-roman|uppercase|url|visible|w-resize|wait|wider|x-fast|x-high|x-large|x-loud|x-low|x-small|x-soft|xx-large|xx-small|yes)\\b" (Arbitrary "Name" :. Arbitrary "Constant")
    , tok "(indigo|gold|firebrick|indianred|darkolivegreen|darkseagreen|mediumvioletred|mediumorchid|chartreuse|mediumslateblue|springgreen|crimson|lightsalmon|brown|turquoise|olivedrab|cyan|skyblue|darkturquoise|goldenrod|darkgreen|darkviolet|darkgray|lightpink|darkmagenta|lightgoldenrodyellow|lavender|yellowgreen|thistle|violet|orchid|ghostwhite|honeydew|cornflowerblue|darkblue|darkkhaki|mediumpurple|cornsilk|bisque|slategray|darkcyan|khaki|wheat|deepskyblue|darkred|steelblue|aliceblue|gainsboro|mediumturquoise|floralwhite|coral|lightgrey|lightcyan|darksalmon|beige|azure|lightsteelblue|oldlace|greenyellow|royalblue|lightseagreen|mistyrose|sienna|lightcoral|orangered|navajowhite|palegreen|burlywood|seashell|mediumspringgreen|papayawhip|blanchedalmond|peru|aquamarine|darkslategray|ivory|dodgerblue|lemonchiffon|chocolate|orange|forestgreen|slateblue|mintcream|antiquewhite|darkorange|cadetblue|moccasin|limegreen|saddlebrown|darkslateblue|lightskyblue|deeppink|plum|darkgoldenrod|sandybrown|magenta|tan|rosybrown|pink|lightblue|palevioletred|mediumseagreen|dimgray|powderblue|seagreen|snow|mediumblue|midnightblue|paleturquoise|palegoldenrod|whitesmoke|darkorchid|salmon|lightslategray|lawngreen|lightgreen|tomato|hotpink|lightyellow|lavenderblush|linen|mediumaquamarine|blueviolet|peachpuff)\\b" (Arbitrary "Name" :. Arbitrary "Entity")
    , tok "(black|silver|gray|white|maroon|red|purple|fuchsia|green|lime|olive|yellow|navy|blue|teal|aqua)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "\\!(important|default)" (Arbitrary "Name" :. Arbitrary "Exception")
    , tok "(true|false)" (Arbitrary "Name" :. Arbitrary "Pseudo")
    , tok "(and|or|not)" (Arbitrary "Operator" :. Arbitrary "Word")
    , tokNext "/\\*" (Arbitrary "Comment" :. Arbitrary "Multiline") (GoTo inlineComment')
    , tok "//[^\\n]*" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "\\#[a-z0-9]{1,6}" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "(-?\\d+)(\\%|[a-z]+)?" (ByGroups [(Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer"), (Arbitrary "Keyword" :. Arbitrary "Type")])
    , tok "(-?\\d*\\.\\d+)(\\%|[a-z]+)?" (ByGroups [(Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float"), (Arbitrary "Keyword" :. Arbitrary "Type")])
    , tokNext "#{" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol") (GoTo interpolation')
    , tok "[\126\\^\\*!&%<>\\|+=@:,./?-]+" (Arbitrary "Operator")
    , tok "[\\[\\]()]+" (Arbitrary "Punctuation")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") (GoTo stringDouble')
    , tokNext "'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single") (GoTo stringSingle')
    , tok "[a-z_-][\\w-]*" (Arbitrary "Name")
    , tok "\\n" (Arbitrary "Text")
    , tokNext "[;{}]" (Arbitrary "Punctuation") (GoTo root')
    ]

for' :: TokenMatcher
for' =
    [ tok "(from|to|through)" (Arbitrary "Operator" :. Arbitrary "Word")
    , anyOf value'
    ]

stringSingle' :: TokenMatcher
stringSingle' =
    [ tok "(\\\\.|#(?=[^\\n{])|[^\\n'#])+" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tokNext "#\\{" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol") (GoTo interpolation')
    , tokNext "'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") Pop
    ]

inlineComment' :: TokenMatcher
inlineComment' =
    [ tok "(\\\\#|#(?=[^{])|\\*(?=[^/])|[^#*])+" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "#\\{" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol") (GoTo interpolation')
    , tokNext "\\*/" (Arbitrary "Comment") Pop
    ]

selector' :: TokenMatcher
selector' =
    [ tok "[ \\t]+" (Arbitrary "Text")
    , tokNext "\\:" (Arbitrary "Name" :. Arbitrary "Decorator") (GoTo pseudoClass')
    , tokNext "\\." (Arbitrary "Name" :. Arbitrary "Class") (GoTo class')
    , tokNext "\\#" (Arbitrary "Name" :. Arbitrary "Namespace") (GoTo id')
    , tok "[a-zA-Z0-9_-]+" (Arbitrary "Name" :. Arbitrary "Tag")
    , tokNext "#\\{" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol") (GoTo interpolation')
    , tok "&" (Arbitrary "Keyword")
    , tok "[\126\\^\\*!&\\[\\]\\(\\)<>\\|+=@:;,./?-]" (Arbitrary "Operator")
    , tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double") (GoTo stringDouble')
    , tokNext "'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single") (GoTo stringSingle')
    , tok "\\n" (Arbitrary "Text")
    , tokNext "[;{}]" (Arbitrary "Punctuation") (GoTo root')
    ]

root' :: TokenMatcher
root' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "//.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/\\*.*?\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tokNext "@import" (Arbitrary "Keyword") (GoTo value')
    , tokNext "@for" (Arbitrary "Keyword") (GoTo for')
    , tokNext "@(debug|warn|if|while)" (Arbitrary "Keyword") (GoTo value')
    , tokNext "(@mixin)( [\\w-]+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Name" :. Arbitrary "Function")]) (GoTo value')
    , tokNext "(@include)( [\\w-]+)" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Name" :. Arbitrary "Decorator")]) (GoTo value')
    , tokNext "@extend" (Arbitrary "Keyword") (GoTo selector')
    , tokNext "@[a-z0-9_-]+" (Arbitrary "Keyword") (GoTo selector')
    , tokNext "(\\$[\\w-]\\w*)([ \\t]*:)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Variable"), (Arbitrary "Operator")]) (GoTo value')
    , tokNext "(?=[^;{}][;}])" (Arbitrary "Name" :. Arbitrary "Attribute") (GoTo attr')
    , tokNext "(?=[^;{}:]+:[^a-z])" (Arbitrary "Name" :. Arbitrary "Attribute") (GoTo attr')
    , tokNext "" (Arbitrary "Text") (GoTo selector')
    ]

id' :: TokenMatcher
id' =
    [ tok "[\\w-]+" (Arbitrary "Name" :. Arbitrary "Namespace")
    , tokNext "#\\{" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol") (GoTo interpolation')
    , tokNext "" (Arbitrary "Text") Pop
    ]

interpolation' :: TokenMatcher
interpolation' =
    [ tokNext "\\}" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Interpol") Pop
    , anyOf value'
    ]

