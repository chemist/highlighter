module Text.Highlighter.Lexers.Matlab (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Matlab"
    , lAliases = ["matlab", "octave"]
    , lExtensions = [".m"]
    , lMimetypes = ["text/matlab"]
    , lStart = root'
    , lFlags = [multiline]
    }

deffunc' :: TokenMatcher
deffunc' =
    [ tokNext "(\\s*)(?:(.+)(\\s*)(=)(\\s*))?(.+)(\\()(.*)(\\))(\\s*)" (ByGroups [(Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Text"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Punctuation"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Punctuation"), (Arbitrary "Text"), (Arbitrary "Punctuation"), (Arbitrary "Text" :. Arbitrary "Whitespace")]) Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "^!.*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Other")
    , tok "%.*$" (Arbitrary "Comment")
    , tokNext "^\\s*function" (Arbitrary "Keyword") (GoTo deffunc')
    , tok "(break|case|catch|classdef|continue|else|elseif|end|enumerated|events|for|function|global|if|methods|otherwise|parfor|persistent|properties|return|spmd|switch|try|while)\\b" (Arbitrary "Keyword")
    , tok "(sin|sind|sinh|asin|asind|asinh|cos|cosd|cosh|acos|acosd|acosh|tan|tand|tanh|atan|atand|atan2|atanh|sec|secd|sech|asec|asecd|asech|csc|cscd|csch|acsc|acscd|acsch|cot|cotd|coth|acot|acotd|acoth|hypot|exp|expm1|log|log1p|log10|log2|pow2|realpow|reallog|realsqrt|sqrt|nthroot|nextpow2|abs|angle|complex|conj|imag|real|unwrap|isreal|cplxpair|fix|floor|ceil|round|mod|rem|sign|airy|besselj|bessely|besselh|besseli|besselk|beta|betainc|betaln|ellipj|ellipke|erf|erfc|erfcx|erfinv|expint|gamma|gammainc|gammaln|psi|legendre|cross|dot|factor|isprime|primes|gcd|lcm|rat|rats|perms|nchoosek|factorial|cart2sph|cart2pol|pol2cart|sph2cart|hsv2rgb|rgb2hsv|zeros|ones|eye|repmat|rand|randn|linspace|logspace|freqspace|meshgrid|accumarray|size|length|ndims|numel|disp|isempty|isequal|isequalwithequalnans|cat|reshape|diag|blkdiag|tril|triu|fliplr|flipud|flipdim|rot90|find|end|sub2ind|ind2sub|bsxfun|ndgrid|permute|ipermute|shiftdim|circshift|squeeze|isscalar|isvector|ans|eps|realmax|realmin|pi|i|inf|nan|isnan|isinf|isfinite|j|why|compan|gallery|hadamard|hankel|hilb|invhilb|magic|pascal|rosser|toeplitz|vander|wilkinson)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "-|==|\126=|<|>|<=|>=|&&|&|\126|\\|\\|?" (Arbitrary "Operator")
    , tok "\\.\\*|\\*|\\+|\\.\\^|\\.\\\\|\\.\\/|\\/|\\\\" (Arbitrary "Operator")
    , tok "\\[|\\]|\\(|\\)|\\{|\\}|:|@|\\.|," (Arbitrary "Punctuation")
    , tok "=|:|;" (Arbitrary "Punctuation")
    , tok "(?<=[\\w\\)\\]])\\'" (Arbitrary "Operator")
    , tokNext "(?<![\\w\\)\\]])\\'" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "[a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name")
    , tok "." (Arbitrary "Text")
    ]

string' :: TokenMatcher
string' =
    [ tokNext "[^\\']*\\'" (Arbitrary "Literal" :. Arbitrary "String") Pop
    ]

