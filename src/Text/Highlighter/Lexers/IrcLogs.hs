module Text.Highlighter.Lexers.IrcLogs (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "IRC logs"
    , lAliases = ["irc"]
    , lExtensions = [".weechatlog"]
    , lMimetypes = ["text/x-irclog"]
    , lStart = root'
    , lFlags = [multiline]
    }

msg' :: TokenMatcher
msg' =
    [ tok "[^\\s]+:(?!//)" (Arbitrary "Name" :. Arbitrary "Attribute")
    , tokNext ".*\\n" (Arbitrary "Text") Pop
    ]

root' :: TokenMatcher
root' =
    [ tok "^\\*\\*\\*\\*(.*)\\*\\*\\*\\*$" (Arbitrary "Comment")
    , tok "^\10        (\10          # irssi / xchat and others\10          (?: \\[|\\()?                  # Opening bracket or paren for the timestamp\10            (?:                        # Timestamp\10                (?: (?:\\d{1,4} [-/]?)+ # Date as - or /-separated groups of digits\10                 [T ])?                # Date/time separator: T or space\10                (?: \\d?\\d [:.]?)+      # Time as :/.-separated groups of 1 or 2 digits\10            )\10          (?: \\]|\\))?\\s+               # Closing bracket or paren for the timestamp\10        |\10          # weechat\10          \\d{4}\\s\\w{3}\\s\\d{2}\\s        # Date\10          \\d{2}:\\d{2}:\\d{2}\\s+         # Time + Whitespace\10        |\10          # xchat\10          \\w{3}\\s\\d{2}\\s               # Date\10          \\d{2}:\\d{2}:\\d{2}\\s+         # Time + Whitespace\10        )?\10    (\\s*<[^>]*>\\s*)$" (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Name" :. Arbitrary "Tag")])
    , tokNext "^\10        (\10          # irssi / xchat and others\10          (?: \\[|\\()?                  # Opening bracket or paren for the timestamp\10            (?:                        # Timestamp\10                (?: (?:\\d{1,4} [-/]?)+ # Date as - or /-separated groups of digits\10                 [T ])?                # Date/time separator: T or space\10                (?: \\d?\\d [:.]?)+      # Time as :/.-separated groups of 1 or 2 digits\10            )\10          (?: \\]|\\))?\\s+               # Closing bracket or paren for the timestamp\10        |\10          # weechat\10          \\d{4}\\s\\w{3}\\s\\d{2}\\s        # Date\10          \\d{2}:\\d{2}:\\d{2}\\s+         # Time + Whitespace\10        |\10          # xchat\10          \\w{3}\\s\\d{2}\\s               # Date\10          \\d{2}:\\d{2}:\\d{2}\\s+         # Time + Whitespace\10        )?\10    \10                (\\s*<.*?>\\s*)          # Nick " (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Name" :. Arbitrary "Tag")]) (GoTo msg')
    , tok "^\10        (\10          # irssi / xchat and others\10          (?: \\[|\\()?                  # Opening bracket or paren for the timestamp\10            (?:                        # Timestamp\10                (?: (?:\\d{1,4} [-/]?)+ # Date as - or /-separated groups of digits\10                 [T ])?                # Date/time separator: T or space\10                (?: \\d?\\d [:.]?)+      # Time as :/.-separated groups of 1 or 2 digits\10            )\10          (?: \\]|\\))?\\s+               # Closing bracket or paren for the timestamp\10        |\10          # weechat\10          \\d{4}\\s\\w{3}\\s\\d{2}\\s        # Date\10          \\d{2}:\\d{2}:\\d{2}\\s+         # Time + Whitespace\10        |\10          # xchat\10          \\w{3}\\s\\d{2}\\s               # Date\10          \\d{2}:\\d{2}:\\d{2}\\s+         # Time + Whitespace\10        )?\10    \10                (\\s*[*]\\s+)            # Star\10                ([^\\s]+\\s+.*?\\n)       # Nick + rest of message " (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Keyword"), (Arbitrary "Generic" :. Arbitrary "Inserted")])
    , tok "^\10        (\10          # irssi / xchat and others\10          (?: \\[|\\()?                  # Opening bracket or paren for the timestamp\10            (?:                        # Timestamp\10                (?: (?:\\d{1,4} [-/]?)+ # Date as - or /-separated groups of digits\10                 [T ])?                # Date/time separator: T or space\10                (?: \\d?\\d [:.]?)+      # Time as :/.-separated groups of 1 or 2 digits\10            )\10          (?: \\]|\\))?\\s+               # Closing bracket or paren for the timestamp\10        |\10          # weechat\10          \\d{4}\\s\\w{3}\\s\\d{2}\\s        # Date\10          \\d{2}:\\d{2}:\\d{2}\\s+         # Time + Whitespace\10        |\10          # xchat\10          \\w{3}\\s\\d{2}\\s               # Date\10          \\d{2}:\\d{2}:\\d{2}\\s+         # Time + Whitespace\10        )?\10    \10                (\\s*(?:\\*{3}|<?-[!@=P]?->?)\\s*)  # Star(s) or symbols\10                ([^\\s]+\\s+)                     # Nick + Space\10                (.*?\\n)                         # Rest of message " (ByGroups [(Arbitrary "Comment" :. Arbitrary "Preproc"), (Arbitrary "Keyword"), (Arbitrary "Literal" :. Arbitrary "String"), (Arbitrary "Comment")])
    , tok "^.*?\\n" (Arbitrary "Text")
    ]

