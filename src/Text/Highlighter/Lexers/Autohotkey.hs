module Text.Highlighter.Lexers.Autohotkey (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "autohotkey"
    , lAliases = ["ahk"]
    , lExtensions = [".ahk", ".ahkl"]
    , lMimetypes = ["text/x-autohotkey"]
    , lStart = root'
    , lFlags = [caseless, multiline, dotall]
    }

commands' :: TokenMatcher
commands' =
    [ tokNext "(autotrim|blockinput|break|click|clipwait|continue|control|controlclick|controlfocus|controlget|controlgetfocus|controlgetpos|controlgettext|controlmove|controlsend|controlsendraw|controlsettext|coordmode|critical|detecthiddentext|detecthiddenwindows|dllcall|drive|driveget|drivespacefree|else|envadd|envdiv|envget|envmult|envset|envsub|envupdate|exit|exitapp|fileappend|filecopy|filecopydir|filecreatedir|filecreateshortcut|filedelete|filegetattrib|filegetshortcut|filegetsize|filegettime|filegetversion|fileinstall|filemove|filemovedir|fileread|filereadline|filerecycle|filerecycleempty|fileremovedir|fileselectfile|fileselectfolder|filesetattrib|filesettime|formattime|gosub|goto|groupactivate|groupadd|groupclose|groupdeactivate|gui|guicontrol|guicontrolget|hotkey|ifexist|ifgreater|ifgreaterorequal|ifinstring|ifless|iflessorequal|ifmsgbox|ifnotequal|ifnotexist|ifnotinstring|ifwinactive|ifwinexist|ifwinnotactive|ifwinnotexist|imagesearch|inidelete|iniread|iniwrite|input|inputbox|keyhistory|keywait|listhotkeys|listlines|listvars|loop|menu|mouseclick|mouseclickdrag|mousegetpos|mousemove|msgbox|onmessage|onexit|outputdebug|pixelgetcolor|pixelsearch|postmessage|process|progress|random|regexmatch|regexreplace|registercallback|regdelete|regread|regwrite|reload|repeat|return|run|runas|runwait|send|sendevent|sendinput|sendmessage|sendmode|sendplay|sendraw|setbatchlines|setcapslockstate|setcontroldelay|setdefaultmousespeed|setenv|setformat|setkeydelay|setmousedelay|setnumlockstate|setscrolllockstate|setstorecapslockmode|settimer|settitlematchmode|setwindelay|setworkingdir|shutdown|sleep|sort|soundbeep|soundget|soundgetwavevolume|soundplay|soundset|soundsetwavevolume|splashimage|splashtextoff|splashtexton|splitpath|statusbargettext|statusbarwait|stringcasesense|stringgetpos|stringleft|stringlen|stringlower|stringmid|stringreplace|stringright|stringsplit|stringtrimleft|stringtrimright|stringupper|suspend|sysget|thread|tooltip|transform|traytip|urldownloadtofile|while|varsetcapacity|winactivate|winactivatebottom|winclose|winget|wingetactivestats|wingetactivetitle|wingetclass|wingetpos|wingettext|wingettitle|winhide|winkill|winmaximize|winmenuselectitem|winminimize|winminimizeall|winminimizeallundo|winmove|winrestore|winset|winsettitle|winshow|winwait|winwaitactive|winwaitclose|winwaitnotactivetrue|false|NULL)\\b" (Arbitrary "Keyword") (GoTo command')
    ]

literals' :: TokenMatcher
literals' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") (GoTo string')
    , tok "A_\\w+" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "%[]\\w#@$?[]+?%" (Arbitrary "Name" :. Arbitrary "Variable")
    , tokNext "[-\126!%^&*+|?:<>/=]=?" (Arbitrary "Operator") (GoTo expressions')
    , tokNext "==" (Arbitrary "Operator") (GoTo expressions')
    , tok "[{()},.%#`;]" (Arbitrary "Punctuation")
    , tok "\\\\" (Arbitrary "Punctuation")
    , anyOf keywords'
    , tok "\\w+" (Arbitrary "Text")
    ]

string' :: TokenMatcher
string' =
    [ tokNext "\"" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , tok "\"\"|`." (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Escape")
    , tok "[^\\`\"\\n]+" (Arbitrary "Literal" :. Arbitrary "String")
    ]

parameters' :: TokenMatcher
parameters' =
    [ tokNext "\\)" (Arbitrary "Punctuation") Pop
    , tokNext "\\(" (Arbitrary "Punctuation") Push
    , anyOf numbers'
    , anyOf literals'
    , anyOf whitespace'
    ]

keynames' :: TokenMatcher
keynames' =
    [ tokNext "\\[[^\\]]+\\]" (Arbitrary "Keyword") (GoTo keynames')
    ]

continuation' :: TokenMatcher
continuation' =
    [ tokNext "\\n\\)" (Arbitrary "Punctuation") Pop
    , tok "\\s[^\\n\\)]+" (Arbitrary "Literal" :. Arbitrary "String")
    ]

labels' :: TokenMatcher
labels' =
    [ tok "(^\\s*)([^:\\s]+?:{1,2})" (ByGroups [(Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Name" :. Arbitrary "Label")])
    , tok "(^\\s*)(::[]\\w#@$?[]+?::)" (ByGroups [(Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Name" :. Arbitrary "Label")])
    ]

comments' :: TokenMatcher
comments' =
    [ tok "^;+.*?$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "(?<=\\s);+.*?$" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "^/\\*.*?\\n\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "(?<!\\n)/\\*.*?\\n\\*/" (Arbitrary "Error")
    ]

command' :: TokenMatcher
command' =
    [ anyOf comments'
    , anyOf whitespace'
    , tokNext "^\\(" (Arbitrary "Literal" :. Arbitrary "String") (GoTo continuation')
    , tokNext "[^\\n]*?(?=;*|$)" (Arbitrary "Literal" :. Arbitrary "String") Pop
    , anyOf numbers'
    , anyOf literals'
    ]

numbers' :: TokenMatcher
numbers' =
    [ tok "(\\d+\\.\\d*|\\d*\\.\\d+)([eE][+-]?[0-9]+)?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "\\d+[eE][+-]?[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0[0-7]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Oct")
    , tok "0[xX][a-fA-F0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "\\d+L" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer" :. Arbitrary "Long")
    , tok "\\d+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    ]

keywords' :: TokenMatcher
keywords' =
    [ tok "(static|global|local)\\b" (Arbitrary "Keyword" :. Arbitrary "Type")
    , tok "(if|else|and|or)\\b" (Arbitrary "Keyword" :. Arbitrary "Reserved")
    ]

directives' :: TokenMatcher
directives' =
    [ tok "#\\w+?\\s" (Arbitrary "Keyword")
    ]

expressions' :: TokenMatcher
expressions' =
    [ anyOf comments'
    , anyOf whitespace'
    , anyOf numbers'
    , anyOf literals'
    , tokNext "([]\\w#@$?[]+)(\\s*)(\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Punctuation")]) (GoTo parameters')
    , tok "A_\\w+" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "%[]\\w#@$?[]+?%" (Arbitrary "Name" :. Arbitrary "Variable")
    , tokNext "{" (Arbitrary "Punctuation") (GoTo block')
    ]

root' :: TokenMatcher
root' =
    [ anyOf whitespace'
    , tokNext "^\\(" (Arbitrary "Literal" :. Arbitrary "String") (GoTo continuation')
    , anyOf comments'
    , tokNext "(^\\s*)(\\w+)(\\s*)(=)" (ByGroups [(Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Name"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Operator")]) (GoTo command')
    , tokNext "([\\w#@$?\\[\\]]+)(\\s*)(\\()" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Text" :. Arbitrary "Whitespace"), (Arbitrary "Punctuation")]) (GoTo parameters')
    , anyOf directives'
    , anyOf labels'
    , anyOf commands'
    , anyOf expressions'
    , anyOf numbers'
    , anyOf literals'
    , anyOf keynames'
    , anyOf keywords'
    ]

block' :: TokenMatcher
block' =
    [ anyOf root'
    , tokNext "{" (Arbitrary "Punctuation") Push
    , tokNext "}" (Arbitrary "Punctuation") Pop
    ]

whitespace' :: TokenMatcher
whitespace' =
    [ tok "[ \\t]+" (Arbitrary "Text" :. Arbitrary "Whitespace")
    ]

