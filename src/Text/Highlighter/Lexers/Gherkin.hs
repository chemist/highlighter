module Text.Highlighter.Lexers.Gherkin (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "Gherkin"
    , lAliases = ["Cucumber", "cucumber", "Gherkin", "gherkin"]
    , lExtensions = [".feature"]
    , lMimetypes = ["text/x-gherkin"]
    , lStart = root'
    , lFlags = [multiline]
    }

double_string' :: TokenMatcher
double_string' =
    [ tokNext "\"" (Arbitrary "Name" :. Arbitrary "Function") Pop
    , anyOf string'
    ]

examples_table_header' :: TokenMatcher
examples_table_header' =
    [ tokNext "\\s+\\|\\s*$" (Arbitrary "Keyword") (PopNum 2)
    , anyOf comments'
    , tok "\\s*\\|" (Arbitrary "Keyword")
    , tok "[^\\|]" (Arbitrary "Name" :. Arbitrary "Variable")
    ]

table_vars' :: TokenMatcher
table_vars' =
    [ tok "(<[^>]+>)" (Arbitrary "Name" :. Arbitrary "Variable")
    ]

string' :: TokenMatcher
string' =
    [ anyOf table_vars'
    , tok "(\\s|.)" (Arbitrary "Literal" :. Arbitrary "String")
    ]

py_string' :: TokenMatcher
py_string' =
    [ tokNext "\"\"\"" (Arbitrary "Keyword") Pop
    , anyOf string'
    ]

root' :: TokenMatcher
root' =
    [ tok "\\n" (Arbitrary "Name" :. Arbitrary "Function")
    , anyOf comments'
    , tokNext "\"\"\"" (Arbitrary "Keyword") (GoTo py_string')
    , tokNext "\\s+\\|" (Arbitrary "Keyword") (GoTo table_content')
    , tokNext "\"" (Arbitrary "Name" :. Arbitrary "Function") (GoTo double_string')
    , anyOf table_vars'
    , anyOf numbers'
    , tok "(\\s*)(@[^@\\r\\n\\t ]+)" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Name" :. Arbitrary "Tag")])
    , tokNext "^(\\s*)(\54616\51648\47564|\51312\44148|\47676\51200|\47564\51068|\47564\50557|\45800|\44536\47532\44256|\44536\47084\47732|\37027\40636|\37027\20040|\32780\19988|\30070|\24403|\21069\25552|\20551\35373|\20551\22914|\20294\26159|\20294\12375|\20006\19988|\12418\12375|\12394\12425\12400|\12383\12384\12375|\12375\12363\12375|\12363\12388|\1608 |\1605\1578\1609 |\1604\1603\1606 |\1593\1606\1583\1605\1575 |\1579\1605 |\1576\1601\1585\1590 |\1575\1584\1575\1611 |\1499\1488\1513\1512 |\1493\1490\1501 |\1489\1492\1497\1504\1514\1503 |\1488\1494\1497 |\1488\1494 |\1488\1489\1500 |\1071\1082\1097\1086 |\1059\1085\1076\1072 |\1058\1086 |\1055\1088\1080\1087\1091\1089\1090\1080\1084\1086, \1097\1086 |\1055\1088\1080\1087\1091\1089\1090\1080\1084\1086 |\1054\1085\1076\1072 |\1053\1086 |\1053\1077\1093\1072\1081 |\1051\1077\1082\1080\1085 |\1050\1086\1075\1072\1090\1086 |\1050\1072\1076\1072 |\1050\1072\1076 |\1050 \1090\1086\1084\1091 \1078\1077 |\1048 |\1047\1072\1076\1072\1090\1086 |\1047\1072\1076\1072\1090\1080 |\1047\1072\1076\1072\1090\1077 |\1045\1089\1083\1080 |\1044\1086\1087\1091\1089\1090\1080\1084 |\1044\1072\1076\1077\1085\1086 |\1042\1072 |\1041\1080\1088\1086\1082 |\1040\1084\1084\1086 |\1040\1083\1080 |\1040\1083\1077 |\1040\1075\1072\1088 |\1040 |\1030 |\536i |\201s |Zatati |Zak\322adaj\261c |Zadato |Zadate |Zadano |Zadani |Zadan |Youse know when youse got |Youse know like when |Yna |Ya know how |Ya gotta |Y |Wun |Wtedy |When y\\'all |When |Wenn |WEN |V\224 |Ve |Und |Un |Th\236 |Then y\\'all |Then |Tapi |Tak |Tada |Tad |S\229 |Stel |Soit |Siis |Si |Sed |Se |Quando |Quand |Quan |Pryd |Pokud |Pokia\318 |Per\242 |Pero |Pak |Oraz |Onda |Ond |Oletetaan |Og |Och |O zaman |N\229r |N\228r |Niin |Nh\432ng |N |Mutta |Men |Mas |Maka |Majd |Mais |Maar |Ma |Lorsque |Lorsqu\\'|Kun |Kuid |Kui |Khi |Ke\271 |Ketika |Kdy\382 |Kaj |Kai |Kada |Kad |Je\380eli |Ja |Ir |I CAN HAZ |I |Ha |Givun |Givet |Given y\\'all |Given |Gitt |Gegeven |Gegeben sei |Fakat |E\287er ki |Etant donn\233 |Et |Ent\227o |Entonces |Entao |En |Eeldades |E |Duota |Dun |Donita\309o |Donat |Donada |Do |Diyelim ki |Dengan |Den youse gotta |De |Dato |Dar |Dann |Dan |Dado |Dac\259 |Daca |DEN |C\226nd |Cuando |Cho |Cept |Cand |Cal |But y\\'all |But |Buh |Bi\7871t |Bet |BUT |At\232s |Atunci |Atesa |Anrhegedig a |Angenommen |And y\\'all |And |An |Ama |Als |Alors |Allora |Ali |Aleshores |Ale |Akkor |Aber |AN |A tak\233 |A |\\* )" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Keyword")]) (GoTo step_content_root')
    , tokNext "^(\44592\45733|\27231\33021|\21151\33021|\12501\12451\12540\12481\12515|\1582\1575\1589\1610\1577|\1514\1499\1493\1504\1492|\1060\1091\1085\1082\1094\1110\1086\1085\1072\1083|\1060\1091\1085\1082\1094\1080\1086\1085\1072\1083\1085\1086\1089\1090|\1060\1091\1085\1082\1094\1080\1086\1085\1072\1083|\1060\1080\1095\1072|\1054\1089\1086\1073\1080\1085\1072|\1052\1086\1075\1091\1115\1085\1086\1089\1090|\214zellik|W\322a\347ciwo\347\263|T\237nh n\259ng|Trajto|Savyb\279|Po\382iadavka|Po\382adavek|Osobina|Ominaisuus|Omadus|OH HAI|Mogu\263nost|Mogucnost|Jellemz\337|F\299\269a|Funzionalit\224|Funktionalit\228t|Funkcionalnost|Funkcionalit\257te|Func\539ionalitate|Functionaliteit|Functionalitate|Funcionalitat|Funcionalidade|Fonctionnalit\233|Fitur|Feature|Egenskap|Egenskab|Crikey|Caracter\237stica|Arwedd)(:)(.*)$" (ByGroups [(Arbitrary "Keyword"), (Arbitrary "Keyword"), (Arbitrary "Name" :. Arbitrary "Function")]) (GoTo narrative')
    , tokNext "^(\\s*)(\49884\45208\47532\50724 \44060\50836|\49884\45208\47532\50724|\48176\44221|\32972\26223|\22580\26223\22823\32177|\22580\26223|\22330\26223\22823\32434|\22330\26223|\21127\26412\22823\32177|\21127\26412|\12486\12531\12503\12524|\12471\12490\12522\12458\12486\12531\12503\12524\12540\12488|\12471\12490\12522\12458\12486\12531\12503\12524|\12471\12490\12522\12458\12450\12454\12488\12521\12452\12531|\12471\12490\12522\12458|\1587\1610\1606\1575\1585\1610\1608 \1605\1582\1591\1591|\1587\1610\1606\1575\1585\1610\1608|\1575\1604\1582\1604\1601\1610\1577|\1514\1512\1495\1497\1513|\1514\1489\1504\1497\1514 \1514\1512\1495\1497\1513|\1512\1511\1506|\1058\1072\1088\1080\1093|\1057\1094\1077\1085\1072\1088\1110\1081|\1057\1094\1077\1085\1072\1088\1080\1086|\1057\1094\1077\1085\1072\1088\1080\1081 \1089\1090\1088\1091\1082\1090\1091\1088\1072\1089\1080|\1057\1094\1077\1085\1072\1088\1080\1081|\1057\1090\1088\1091\1082\1090\1091\1088\1072 \1089\1094\1077\1085\1072\1088\1110\1102|\1057\1090\1088\1091\1082\1090\1091\1088\1072 \1089\1094\1077\1085\1072\1088\1080\1112\1072|\1057\1090\1088\1091\1082\1090\1091\1088\1072 \1089\1094\1077\1085\1072\1088\1080\1103|\1057\1082\1080\1094\1072|\1056\1072\1084\1082\1072 \1085\1072 \1089\1094\1077\1085\1072\1088\1080\1081|\1055\1088\1080\1084\1077\1088|\1055\1088\1077\1076\1099\1089\1090\1086\1088\1080\1103|\1055\1088\1077\1076\1080\1089\1090\1086\1088\1080\1103|\1055\1086\1079\1072\1076\1080\1085\1072|\1055\1077\1088\1077\1076\1091\1084\1086\1074\1072|\1054\1089\1085\1086\1074\1072|\1050\1086\1085\1094\1077\1087\1090|\1050\1086\1085\1090\1077\1082\1089\1090|Za\322o\380enia|Wharrimean is|T\236nh hu\7889ng|The thing of it is|Tausta|Taust|Tapausaihio|Tapaus|Szenariogrundriss|Szenario|Szablon scenariusza|Stsenaarium|Struktura scenarija|Skica|Skenario konsep|Skenario|Situ\257cija|Senaryo tasla\287\305|Senaryo|Sc\233n\225\345|Sc\233nario|Schema dello scenario|Scen\257rijs p\275c parauga|Scen\257rijs|Scen\225r|Scenaro|Scenariusz|Scenariul de \351ablon|Scenariul de sablon|Scenariu|Scenario Outline|Scenario Amlinellol|Scenario|Scenarijus|Scenarijaus \353ablonas|Scenarij|Scenarie|Rerefons|Raamstsenaarium|Primer|Pozad\237|Pozadina|Pozadie|Plan du sc\233nario|Plan du Sc\233nario|Osnova sc\233n\225\345e|Osnova|N\225\269rt Sc\233n\225\345e|N\225\269rt Scen\225ru|Mate|MISHUN SRSLY|MISHUN|K\7883ch b\7843n|Konturo de la scenaro|Kontext|Konteksts|Kontekstas|Kontekst|Koncept|Khung t\236nh hu\7889ng|Khung k\7883ch b\7843n|H\225tt\233r|Grundlage|Ge\231mi\351|Forgat\243k\246nyv v\225zlat|Forgat\243k\246nyv|Fono|Esquema do Cen\225rio|Esquema do Cenario|Esquema del escenario|Esquema de l\\'escenari|Escenario|Escenari|Dis is what went down|Dasar|Contexto|Contexte|Contesto|Condi\355ii|Conditii|Cen\225rio|Cenario|Cefndir|B\7889i c\7843nh|Blokes|Bakgrunn|Bakgrund|Baggrund|Background|B4|Antecedents|Antecedentes|All y\\'all|Achtergrond|Abstrakt Scenario|Abstract Scenario)(:)(.*)$" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Keyword"), (Arbitrary "Keyword"), (Arbitrary "Name" :. Arbitrary "Function")]) (GoTo feature_elements')
    , tokNext "^(\\s*)(\50696|\20363\23376|\20363|\12469\12531\12503\12523|\1575\1605\1579\1604\1577|\1491\1493\1490\1502\1488\1493\1514|\1057\1094\1077\1085\1072\1088\1080\1112\1080|\1055\1088\1080\1084\1077\1088\1080|\1055\1088\1080\1082\1083\1072\1076\1080|\1052\1080\1089\1086\1083\1083\1072\1088|\1047\1085\1072\1095\1077\1085\1080\1103|\214rnekler|Voorbeelden|Variantai|Tapaukset|Scenarios|Scenariji|Scenarijai|P\345\237klady|P\233ld\225k|Pr\237klady|Przyk\322ady|Primjeri|Primeri|Piem\275ri|Pavyzd\382iai|Paraugs|Juhtumid|Exemplos|Exemples|Exemplele|Exempel|Examples|Esempi|Enghreifftiau|Ekzemploj|Eksempler|Ejemplos|EXAMPLZ|D\7919 li\7879u|Contoh|Cobber|Beispiele)(:)(.*)$" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Keyword"), (Arbitrary "Keyword"), (Arbitrary "Name" :. Arbitrary "Function")]) (GoTo examples_table')
    , tok "(\\s|.)" (Arbitrary "Name" :. Arbitrary "Function")
    ]

examples_table' :: TokenMatcher
examples_table' =
    [ tokNext "\\s+\\|" (Arbitrary "Keyword") (GoTo examples_table_header')
    , anyOf comments'
    , tok "(\\s|.)" (Arbitrary "Name" :. Arbitrary "Function")
    ]

comments' :: TokenMatcher
comments' =
    [ tok "#.*$" (Arbitrary "Comment")
    ]

scenario_sections_on_stack' :: TokenMatcher
scenario_sections_on_stack' =
    [ tokNext "^(\\s*)(\49884\45208\47532\50724 \44060\50836|\49884\45208\47532\50724|\48176\44221|\32972\26223|\22580\26223\22823\32177|\22580\26223|\22330\26223\22823\32434|\22330\26223|\21127\26412\22823\32177|\21127\26412|\12486\12531\12503\12524|\12471\12490\12522\12458\12486\12531\12503\12524\12540\12488|\12471\12490\12522\12458\12486\12531\12503\12524|\12471\12490\12522\12458\12450\12454\12488\12521\12452\12531|\12471\12490\12522\12458|\1587\1610\1606\1575\1585\1610\1608 \1605\1582\1591\1591|\1587\1610\1606\1575\1585\1610\1608|\1575\1604\1582\1604\1601\1610\1577|\1514\1512\1495\1497\1513|\1514\1489\1504\1497\1514 \1514\1512\1495\1497\1513|\1512\1511\1506|\1058\1072\1088\1080\1093|\1057\1094\1077\1085\1072\1088\1110\1081|\1057\1094\1077\1085\1072\1088\1080\1086|\1057\1094\1077\1085\1072\1088\1080\1081 \1089\1090\1088\1091\1082\1090\1091\1088\1072\1089\1080|\1057\1094\1077\1085\1072\1088\1080\1081|\1057\1090\1088\1091\1082\1090\1091\1088\1072 \1089\1094\1077\1085\1072\1088\1110\1102|\1057\1090\1088\1091\1082\1090\1091\1088\1072 \1089\1094\1077\1085\1072\1088\1080\1112\1072|\1057\1090\1088\1091\1082\1090\1091\1088\1072 \1089\1094\1077\1085\1072\1088\1080\1103|\1057\1082\1080\1094\1072|\1056\1072\1084\1082\1072 \1085\1072 \1089\1094\1077\1085\1072\1088\1080\1081|\1055\1088\1080\1084\1077\1088|\1055\1088\1077\1076\1099\1089\1090\1086\1088\1080\1103|\1055\1088\1077\1076\1080\1089\1090\1086\1088\1080\1103|\1055\1086\1079\1072\1076\1080\1085\1072|\1055\1077\1088\1077\1076\1091\1084\1086\1074\1072|\1054\1089\1085\1086\1074\1072|\1050\1086\1085\1094\1077\1087\1090|\1050\1086\1085\1090\1077\1082\1089\1090|Za\322o\380enia|Wharrimean is|T\236nh hu\7889ng|The thing of it is|Tausta|Taust|Tapausaihio|Tapaus|Szenariogrundriss|Szenario|Szablon scenariusza|Stsenaarium|Struktura scenarija|Skica|Skenario konsep|Skenario|Situ\257cija|Senaryo tasla\287\305|Senaryo|Sc\233n\225\345|Sc\233nario|Schema dello scenario|Scen\257rijs p\275c parauga|Scen\257rijs|Scen\225r|Scenaro|Scenariusz|Scenariul de \351ablon|Scenariul de sablon|Scenariu|Scenario Outline|Scenario Amlinellol|Scenario|Scenarijus|Scenarijaus \353ablonas|Scenarij|Scenarie|Rerefons|Raamstsenaarium|Primer|Pozad\237|Pozadina|Pozadie|Plan du sc\233nario|Plan du Sc\233nario|Osnova sc\233n\225\345e|Osnova|N\225\269rt Sc\233n\225\345e|N\225\269rt Scen\225ru|Mate|MISHUN SRSLY|MISHUN|K\7883ch b\7843n|Konturo de la scenaro|Kontext|Konteksts|Kontekstas|Kontekst|Koncept|Khung t\236nh hu\7889ng|Khung k\7883ch b\7843n|H\225tt\233r|Grundlage|Ge\231mi\351|Forgat\243k\246nyv v\225zlat|Forgat\243k\246nyv|Fono|Esquema do Cen\225rio|Esquema do Cenario|Esquema del escenario|Esquema de l\\'escenari|Escenario|Escenari|Dis is what went down|Dasar|Contexto|Contexte|Contesto|Condi\355ii|Conditii|Cen\225rio|Cenario|Cefndir|B\7889i c\7843nh|Blokes|Bakgrunn|Bakgrund|Baggrund|Background|B4|Antecedents|Antecedentes|All y\\'all|Achtergrond|Abstrakt Scenario|Abstract Scenario)(:)(.*)$" (ByGroups [(Arbitrary "Name" :. Arbitrary "Function"), (Arbitrary "Keyword"), (Arbitrary "Keyword"), (Arbitrary "Name" :. Arbitrary "Function")]) (GoTo feature_elements_on_stack')
    ]

step_content_root' :: TokenMatcher
step_content_root' =
    [ tokNext "$" (Arbitrary "Keyword") Pop
    , anyOf step_content'
    ]

numbers' :: TokenMatcher
numbers' =
    [ tok "(\\d+\\.?\\d*|\\d*\\.\\d+)([eE][+-]?[0-9]+)?" (Arbitrary "Literal" :. Arbitrary "String")
    ]

feature_elements' :: TokenMatcher
feature_elements' =
    [ tokNext "^(\\s*)(\54616\51648\47564|\51312\44148|\47676\51200|\47564\51068|\47564\50557|\45800|\44536\47532\44256|\44536\47084\47732|\37027\40636|\37027\20040|\32780\19988|\30070|\24403|\21069\25552|\20551\35373|\20551\22914|\20294\26159|\20294\12375|\20006\19988|\12418\12375|\12394\12425\12400|\12383\12384\12375|\12375\12363\12375|\12363\12388|\1608 |\1605\1578\1609 |\1604\1603\1606 |\1593\1606\1583\1605\1575 |\1579\1605 |\1576\1601\1585\1590 |\1575\1584\1575\1611 |\1499\1488\1513\1512 |\1493\1490\1501 |\1489\1492\1497\1504\1514\1503 |\1488\1494\1497 |\1488\1494 |\1488\1489\1500 |\1071\1082\1097\1086 |\1059\1085\1076\1072 |\1058\1086 |\1055\1088\1080\1087\1091\1089\1090\1080\1084\1086, \1097\1086 |\1055\1088\1080\1087\1091\1089\1090\1080\1084\1086 |\1054\1085\1076\1072 |\1053\1086 |\1053\1077\1093\1072\1081 |\1051\1077\1082\1080\1085 |\1050\1086\1075\1072\1090\1086 |\1050\1072\1076\1072 |\1050\1072\1076 |\1050 \1090\1086\1084\1091 \1078\1077 |\1048 |\1047\1072\1076\1072\1090\1086 |\1047\1072\1076\1072\1090\1080 |\1047\1072\1076\1072\1090\1077 |\1045\1089\1083\1080 |\1044\1086\1087\1091\1089\1090\1080\1084 |\1044\1072\1076\1077\1085\1086 |\1042\1072 |\1041\1080\1088\1086\1082 |\1040\1084\1084\1086 |\1040\1083\1080 |\1040\1083\1077 |\1040\1075\1072\1088 |\1040 |\1030 |\536i |\201s |Zatati |Zak\322adaj\261c |Zadato |Zadate |Zadano |Zadani |Zadan |Youse know when youse got |Youse know like when |Yna |Ya know how |Ya gotta |Y |Wun |Wtedy |When y\\'all |When |Wenn |WEN |V\224 |Ve |Und |Un |Th\236 |Then y\\'all |Then |Tapi |Tak |Tada |Tad |S\229 |Stel |Soit |Siis |Si |Sed |Se |Quando |Quand |Quan |Pryd |Pokud |Pokia\318 |Per\242 |Pero |Pak |Oraz |Onda |Ond |Oletetaan |Og |Och |O zaman |N\229r |N\228r |Niin |Nh\432ng |N |Mutta |Men |Mas |Maka |Majd |Mais |Maar |Ma |Lorsque |Lorsqu\\'|Kun |Kuid |Kui |Khi |Ke\271 |Ketika |Kdy\382 |Kaj |Kai |Kada |Kad |Je\380eli |Ja |Ir |I CAN HAZ |I |Ha |Givun |Givet |Given y\\'all |Given |Gitt |Gegeven |Gegeben sei |Fakat |E\287er ki |Etant donn\233 |Et |Ent\227o |Entonces |Entao |En |Eeldades |E |Duota |Dun |Donita\309o |Donat |Donada |Do |Diyelim ki |Dengan |Den youse gotta |De |Dato |Dar |Dann |Dan |Dado |Dac\259 |Daca |DEN |C\226nd |Cuando |Cho |Cept |Cand |Cal |But y\\'all |But |Buh |Bi\7871t |Bet |BUT |At\232s |Atunci |Atesa |Anrhegedig a |Angenommen |And y\\'all |And |An |Ama |Als |Alors |Allora |Ali |Aleshores |Ale |Akkor |Aber |AN |A tak\233 |A |\\* )" (Arbitrary "Keyword") (GoTo step_content_stack')
    , anyOf comments'
    , tok "(\\s|.)" (Arbitrary "Name" :. Arbitrary "Function")
    ]

narrative' :: TokenMatcher
narrative' =
    [ anyOf scenario_sections_on_stack'
    , tok "(\\s|.)" (Arbitrary "Name" :. Arbitrary "Function")
    ]

feature_elements_on_stack' :: TokenMatcher
feature_elements_on_stack' =
    [ tokNext "^(\\s*)(\54616\51648\47564|\51312\44148|\47676\51200|\47564\51068|\47564\50557|\45800|\44536\47532\44256|\44536\47084\47732|\37027\40636|\37027\20040|\32780\19988|\30070|\24403|\21069\25552|\20551\35373|\20551\22914|\20294\26159|\20294\12375|\20006\19988|\12418\12375|\12394\12425\12400|\12383\12384\12375|\12375\12363\12375|\12363\12388|\1608 |\1605\1578\1609 |\1604\1603\1606 |\1593\1606\1583\1605\1575 |\1579\1605 |\1576\1601\1585\1590 |\1575\1584\1575\1611 |\1499\1488\1513\1512 |\1493\1490\1501 |\1489\1492\1497\1504\1514\1503 |\1488\1494\1497 |\1488\1494 |\1488\1489\1500 |\1071\1082\1097\1086 |\1059\1085\1076\1072 |\1058\1086 |\1055\1088\1080\1087\1091\1089\1090\1080\1084\1086, \1097\1086 |\1055\1088\1080\1087\1091\1089\1090\1080\1084\1086 |\1054\1085\1076\1072 |\1053\1086 |\1053\1077\1093\1072\1081 |\1051\1077\1082\1080\1085 |\1050\1086\1075\1072\1090\1086 |\1050\1072\1076\1072 |\1050\1072\1076 |\1050 \1090\1086\1084\1091 \1078\1077 |\1048 |\1047\1072\1076\1072\1090\1086 |\1047\1072\1076\1072\1090\1080 |\1047\1072\1076\1072\1090\1077 |\1045\1089\1083\1080 |\1044\1086\1087\1091\1089\1090\1080\1084 |\1044\1072\1076\1077\1085\1086 |\1042\1072 |\1041\1080\1088\1086\1082 |\1040\1084\1084\1086 |\1040\1083\1080 |\1040\1083\1077 |\1040\1075\1072\1088 |\1040 |\1030 |\536i |\201s |Zatati |Zak\322adaj\261c |Zadato |Zadate |Zadano |Zadani |Zadan |Youse know when youse got |Youse know like when |Yna |Ya know how |Ya gotta |Y |Wun |Wtedy |When y\\'all |When |Wenn |WEN |V\224 |Ve |Und |Un |Th\236 |Then y\\'all |Then |Tapi |Tak |Tada |Tad |S\229 |Stel |Soit |Siis |Si |Sed |Se |Quando |Quand |Quan |Pryd |Pokud |Pokia\318 |Per\242 |Pero |Pak |Oraz |Onda |Ond |Oletetaan |Og |Och |O zaman |N\229r |N\228r |Niin |Nh\432ng |N |Mutta |Men |Mas |Maka |Majd |Mais |Maar |Ma |Lorsque |Lorsqu\\'|Kun |Kuid |Kui |Khi |Ke\271 |Ketika |Kdy\382 |Kaj |Kai |Kada |Kad |Je\380eli |Ja |Ir |I CAN HAZ |I |Ha |Givun |Givet |Given y\\'all |Given |Gitt |Gegeven |Gegeben sei |Fakat |E\287er ki |Etant donn\233 |Et |Ent\227o |Entonces |Entao |En |Eeldades |E |Duota |Dun |Donita\309o |Donat |Donada |Do |Diyelim ki |Dengan |Den youse gotta |De |Dato |Dar |Dann |Dan |Dado |Dac\259 |Daca |DEN |C\226nd |Cuando |Cho |Cept |Cand |Cal |But y\\'all |But |Buh |Bi\7871t |Bet |BUT |At\232s |Atunci |Atesa |Anrhegedig a |Angenommen |And y\\'all |And |An |Ama |Als |Alors |Allora |Ali |Aleshores |Ale |Akkor |Aber |AN |A tak\233 |A |\\* )" (Arbitrary "Keyword") (PopNum 2)
    , anyOf comments'
    , tok "(\\s|.)" (Arbitrary "Name" :. Arbitrary "Function")
    ]

table_content' :: TokenMatcher
table_content' =
    [ tokNext "\\s+\\|\\s*$" (Arbitrary "Keyword") Pop
    , anyOf comments'
    , tok "\\s*\\|" (Arbitrary "Keyword")
    , anyOf string'
    ]

step_content' :: TokenMatcher
step_content' =
    [ tokNext "\"" (Arbitrary "Name" :. Arbitrary "Function") (GoTo double_string')
    , anyOf table_vars'
    , anyOf numbers'
    , anyOf comments'
    , tok "(\\s|.)" (Arbitrary "Name" :. Arbitrary "Function")
    ]

step_content_stack' :: TokenMatcher
step_content_stack' =
    [ tokNext "$" (Arbitrary "Keyword") (PopNum 2)
    , anyOf step_content'
    ]

