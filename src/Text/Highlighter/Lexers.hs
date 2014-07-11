module Text.Highlighter.Lexers where

import Text.Highlighter.Types

import qualified Text.Highlighter.Lexers.ABAP
import qualified Text.Highlighter.Lexers.ActionScript
import qualified Text.Highlighter.Lexers.ActionScript3
import qualified Text.Highlighter.Lexers.Ada
import qualified Text.Highlighter.Lexers.Antlr
import qualified Text.Highlighter.Lexers.ApacheConf
import qualified Text.Highlighter.Lexers.AppleScript
import qualified Text.Highlighter.Lexers.Asymptote
import qualified Text.Highlighter.Lexers.Atomo
import qualified Text.Highlighter.Lexers.Autohotkey
import qualified Text.Highlighter.Lexers.Bash
import qualified Text.Highlighter.Lexers.Batch
import qualified Text.Highlighter.Lexers.BBCode
import qualified Text.Highlighter.Lexers.Befunge
import qualified Text.Highlighter.Lexers.BlitzMax
import qualified Text.Highlighter.Lexers.Boo
import qualified Text.Highlighter.Lexers.Brainfuck
import qualified Text.Highlighter.Lexers.C
import qualified Text.Highlighter.Lexers.Clojure
import qualified Text.Highlighter.Lexers.CMake
import qualified Text.Highlighter.Lexers.CoffeeScript
import qualified Text.Highlighter.Lexers.Coldfusion
import qualified Text.Highlighter.Lexers.CommonLisp
import qualified Text.Highlighter.Lexers.Cpp
import qualified Text.Highlighter.Lexers.Css
import qualified Text.Highlighter.Lexers.Cython
import qualified Text.Highlighter.Lexers.D
import qualified Text.Highlighter.Lexers.DarcsPatch
import qualified Text.Highlighter.Lexers.DebianControl
import qualified Text.Highlighter.Lexers.Diff
import qualified Text.Highlighter.Lexers.Django
import qualified Text.Highlighter.Lexers.Duel
import qualified Text.Highlighter.Lexers.Dylan
import qualified Text.Highlighter.Lexers.Erlang
import qualified Text.Highlighter.Lexers.Factor
import qualified Text.Highlighter.Lexers.Felix
import qualified Text.Highlighter.Lexers.Fortran
import qualified Text.Highlighter.Lexers.Gas
import qualified Text.Highlighter.Lexers.GenshiText
import qualified Text.Highlighter.Lexers.Gettext
import qualified Text.Highlighter.Lexers.Gherkin
import qualified Text.Highlighter.Lexers.GLShader
import qualified Text.Highlighter.Lexers.Gnuplot
import qualified Text.Highlighter.Lexers.Go
import qualified Text.Highlighter.Lexers.GoodDataCL
import qualified Text.Highlighter.Lexers.Groff
import qualified Text.Highlighter.Lexers.Haskell
import qualified Text.Highlighter.Lexers.Haxe
import qualified Text.Highlighter.Lexers.Html
import qualified Text.Highlighter.Lexers.Hybris
import qualified Text.Highlighter.Lexers.Ini
import qualified Text.Highlighter.Lexers.Io
import qualified Text.Highlighter.Lexers.Ioke
import qualified Text.Highlighter.Lexers.IrcLogs
import qualified Text.Highlighter.Lexers.Java
import qualified Text.Highlighter.Lexers.Javascript
import qualified Text.Highlighter.Lexers.LighttpdConf
import qualified Text.Highlighter.Lexers.Llvm
import qualified Text.Highlighter.Lexers.Logtalk
import qualified Text.Highlighter.Lexers.Lua
import qualified Text.Highlighter.Lexers.Mako
import qualified Text.Highlighter.Lexers.Maql
import qualified Text.Highlighter.Lexers.Matlab
import qualified Text.Highlighter.Lexers.MiniD
import qualified Text.Highlighter.Lexers.Modelica
import qualified Text.Highlighter.Lexers.Modula2
import qualified Text.Highlighter.Lexers.MoinWiki
import qualified Text.Highlighter.Lexers.MOOCode
import qualified Text.Highlighter.Lexers.MuPAD
import qualified Text.Highlighter.Lexers.Mxml
import qualified Text.Highlighter.Lexers.MySql
import qualified Text.Highlighter.Lexers.Nasm
import qualified Text.Highlighter.Lexers.Newspeak
import qualified Text.Highlighter.Lexers.NginxConf
import qualified Text.Highlighter.Lexers.NumPy
import qualified Text.Highlighter.Lexers.Objdump
import qualified Text.Highlighter.Lexers.ObjectiveC
import qualified Text.Highlighter.Lexers.ObjectiveJ
import qualified Text.Highlighter.Lexers.Ocaml
import qualified Text.Highlighter.Lexers.Ooc
import qualified Text.Highlighter.Lexers.Perl
import qualified Text.Highlighter.Lexers.Php
import qualified Text.Highlighter.Lexers.PostScript
import qualified Text.Highlighter.Lexers.Povray
import qualified Text.Highlighter.Lexers.Prolog
import qualified Text.Highlighter.Lexers.Properties
import qualified Text.Highlighter.Lexers.ProtoBuf
import qualified Text.Highlighter.Lexers.Python
import qualified Text.Highlighter.Lexers.Python3
import qualified Text.Highlighter.Lexers.Python3Traceback
import qualified Text.Highlighter.Lexers.PythonTraceback
import qualified Text.Highlighter.Lexers.Ragel
import qualified Text.Highlighter.Lexers.RagelEmbedded
import qualified Text.Highlighter.Lexers.Redcode
import qualified Text.Highlighter.Lexers.S
import qualified Text.Highlighter.Lexers.Scala
import qualified Text.Highlighter.Lexers.Scheme
import qualified Text.Highlighter.Lexers.Scss
import qualified Text.Highlighter.Lexers.Smalltalk
import qualified Text.Highlighter.Lexers.Smarty
import qualified Text.Highlighter.Lexers.SourcesList
import qualified Text.Highlighter.Lexers.Sql
import qualified Text.Highlighter.Lexers.SquidConf
import qualified Text.Highlighter.Lexers.Tcl
import qualified Text.Highlighter.Lexers.Tcsh
import qualified Text.Highlighter.Lexers.Tex
import qualified Text.Highlighter.Lexers.Vala
import qualified Text.Highlighter.Lexers.VbNet
import qualified Text.Highlighter.Lexers.Velocity
import qualified Text.Highlighter.Lexers.Verilog
import qualified Text.Highlighter.Lexers.Vim
import qualified Text.Highlighter.Lexers.Xml
import qualified Text.Highlighter.Lexers.Xslt


lexers :: [(String, Lexer)]
lexers = concat
    [ map (\e -> (e, Text.Highlighter.Lexers.ABAP.lexer)) (lExtensions Text.Highlighter.Lexers.ABAP.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.ActionScript.lexer)) (lExtensions Text.Highlighter.Lexers.ActionScript.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.ActionScript3.lexer)) (lExtensions Text.Highlighter.Lexers.ActionScript3.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Ada.lexer)) (lExtensions Text.Highlighter.Lexers.Ada.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Antlr.lexer)) (lExtensions Text.Highlighter.Lexers.Antlr.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.ApacheConf.lexer)) (lExtensions Text.Highlighter.Lexers.ApacheConf.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.AppleScript.lexer)) (lExtensions Text.Highlighter.Lexers.AppleScript.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Asymptote.lexer)) (lExtensions Text.Highlighter.Lexers.Asymptote.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Atomo.lexer)) (lExtensions Text.Highlighter.Lexers.Atomo.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Autohotkey.lexer)) (lExtensions Text.Highlighter.Lexers.Autohotkey.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Bash.lexer)) (lExtensions Text.Highlighter.Lexers.Bash.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Batch.lexer)) (lExtensions Text.Highlighter.Lexers.Batch.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.BBCode.lexer)) (lExtensions Text.Highlighter.Lexers.BBCode.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Befunge.lexer)) (lExtensions Text.Highlighter.Lexers.Befunge.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.BlitzMax.lexer)) (lExtensions Text.Highlighter.Lexers.BlitzMax.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Boo.lexer)) (lExtensions Text.Highlighter.Lexers.Boo.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Brainfuck.lexer)) (lExtensions Text.Highlighter.Lexers.Brainfuck.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.C.lexer)) (lExtensions Text.Highlighter.Lexers.C.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Clojure.lexer)) (lExtensions Text.Highlighter.Lexers.Clojure.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.CMake.lexer)) (lExtensions Text.Highlighter.Lexers.CMake.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.CoffeeScript.lexer)) (lExtensions Text.Highlighter.Lexers.CoffeeScript.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Coldfusion.lexer)) (lExtensions Text.Highlighter.Lexers.Coldfusion.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.CommonLisp.lexer)) (lExtensions Text.Highlighter.Lexers.CommonLisp.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Cpp.lexer)) (lExtensions Text.Highlighter.Lexers.Cpp.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Css.lexer)) (lExtensions Text.Highlighter.Lexers.Css.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Cython.lexer)) (lExtensions Text.Highlighter.Lexers.Cython.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.D.lexer)) (lExtensions Text.Highlighter.Lexers.D.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.DarcsPatch.lexer)) (lExtensions Text.Highlighter.Lexers.DarcsPatch.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.DebianControl.lexer)) (lExtensions Text.Highlighter.Lexers.DebianControl.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Diff.lexer)) (lExtensions Text.Highlighter.Lexers.Diff.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Django.lexer)) (lExtensions Text.Highlighter.Lexers.Django.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Duel.lexer)) (lExtensions Text.Highlighter.Lexers.Duel.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Dylan.lexer)) (lExtensions Text.Highlighter.Lexers.Dylan.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Erlang.lexer)) (lExtensions Text.Highlighter.Lexers.Erlang.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Factor.lexer)) (lExtensions Text.Highlighter.Lexers.Factor.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Felix.lexer)) (lExtensions Text.Highlighter.Lexers.Felix.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Fortran.lexer)) (lExtensions Text.Highlighter.Lexers.Fortran.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Gas.lexer)) (lExtensions Text.Highlighter.Lexers.Gas.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.GenshiText.lexer)) (lExtensions Text.Highlighter.Lexers.GenshiText.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Gettext.lexer)) (lExtensions Text.Highlighter.Lexers.Gettext.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Gherkin.lexer)) (lExtensions Text.Highlighter.Lexers.Gherkin.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.GLShader.lexer)) (lExtensions Text.Highlighter.Lexers.GLShader.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Gnuplot.lexer)) (lExtensions Text.Highlighter.Lexers.Gnuplot.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Go.lexer)) (lExtensions Text.Highlighter.Lexers.Go.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.GoodDataCL.lexer)) (lExtensions Text.Highlighter.Lexers.GoodDataCL.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Groff.lexer)) (lExtensions Text.Highlighter.Lexers.Groff.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Haskell.lexer)) (lExtensions Text.Highlighter.Lexers.Haskell.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Haxe.lexer)) (lExtensions Text.Highlighter.Lexers.Haxe.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Html.lexer)) (lExtensions Text.Highlighter.Lexers.Html.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Hybris.lexer)) (lExtensions Text.Highlighter.Lexers.Hybris.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Ini.lexer)) (lExtensions Text.Highlighter.Lexers.Ini.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Io.lexer)) (lExtensions Text.Highlighter.Lexers.Io.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Ioke.lexer)) (lExtensions Text.Highlighter.Lexers.Ioke.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.IrcLogs.lexer)) (lExtensions Text.Highlighter.Lexers.IrcLogs.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Java.lexer)) (lExtensions Text.Highlighter.Lexers.Java.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Javascript.lexer)) (lExtensions Text.Highlighter.Lexers.Javascript.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.LighttpdConf.lexer)) (lExtensions Text.Highlighter.Lexers.LighttpdConf.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Llvm.lexer)) (lExtensions Text.Highlighter.Lexers.Llvm.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Logtalk.lexer)) (lExtensions Text.Highlighter.Lexers.Logtalk.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Lua.lexer)) (lExtensions Text.Highlighter.Lexers.Lua.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Mako.lexer)) (lExtensions Text.Highlighter.Lexers.Mako.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Maql.lexer)) (lExtensions Text.Highlighter.Lexers.Maql.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Matlab.lexer)) (lExtensions Text.Highlighter.Lexers.Matlab.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.MiniD.lexer)) (lExtensions Text.Highlighter.Lexers.MiniD.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Modelica.lexer)) (lExtensions Text.Highlighter.Lexers.Modelica.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Modula2.lexer)) (lExtensions Text.Highlighter.Lexers.Modula2.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.MoinWiki.lexer)) (lExtensions Text.Highlighter.Lexers.MoinWiki.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.MOOCode.lexer)) (lExtensions Text.Highlighter.Lexers.MOOCode.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.MuPAD.lexer)) (lExtensions Text.Highlighter.Lexers.MuPAD.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Mxml.lexer)) (lExtensions Text.Highlighter.Lexers.Mxml.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.MySql.lexer)) (lExtensions Text.Highlighter.Lexers.MySql.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Nasm.lexer)) (lExtensions Text.Highlighter.Lexers.Nasm.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Newspeak.lexer)) (lExtensions Text.Highlighter.Lexers.Newspeak.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.NginxConf.lexer)) (lExtensions Text.Highlighter.Lexers.NginxConf.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.NumPy.lexer)) (lExtensions Text.Highlighter.Lexers.NumPy.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Objdump.lexer)) (lExtensions Text.Highlighter.Lexers.Objdump.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.ObjectiveC.lexer)) (lExtensions Text.Highlighter.Lexers.ObjectiveC.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.ObjectiveJ.lexer)) (lExtensions Text.Highlighter.Lexers.ObjectiveJ.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Ocaml.lexer)) (lExtensions Text.Highlighter.Lexers.Ocaml.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Ooc.lexer)) (lExtensions Text.Highlighter.Lexers.Ooc.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Perl.lexer)) (lExtensions Text.Highlighter.Lexers.Perl.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Php.lexer)) (lExtensions Text.Highlighter.Lexers.Php.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.PostScript.lexer)) (lExtensions Text.Highlighter.Lexers.PostScript.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Povray.lexer)) (lExtensions Text.Highlighter.Lexers.Povray.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Prolog.lexer)) (lExtensions Text.Highlighter.Lexers.Prolog.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Properties.lexer)) (lExtensions Text.Highlighter.Lexers.Properties.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.ProtoBuf.lexer)) (lExtensions Text.Highlighter.Lexers.ProtoBuf.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Python.lexer)) (lExtensions Text.Highlighter.Lexers.Python.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Python3.lexer)) (lExtensions Text.Highlighter.Lexers.Python3.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Python3Traceback.lexer)) (lExtensions Text.Highlighter.Lexers.Python3Traceback.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.PythonTraceback.lexer)) (lExtensions Text.Highlighter.Lexers.PythonTraceback.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Ragel.lexer)) (lExtensions Text.Highlighter.Lexers.Ragel.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.RagelEmbedded.lexer)) (lExtensions Text.Highlighter.Lexers.RagelEmbedded.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Redcode.lexer)) (lExtensions Text.Highlighter.Lexers.Redcode.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.S.lexer)) (lExtensions Text.Highlighter.Lexers.S.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Scala.lexer)) (lExtensions Text.Highlighter.Lexers.Scala.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Scheme.lexer)) (lExtensions Text.Highlighter.Lexers.Scheme.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Scss.lexer)) (lExtensions Text.Highlighter.Lexers.Scss.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Smalltalk.lexer)) (lExtensions Text.Highlighter.Lexers.Smalltalk.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Smarty.lexer)) (lExtensions Text.Highlighter.Lexers.Smarty.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.SourcesList.lexer)) (lExtensions Text.Highlighter.Lexers.SourcesList.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Sql.lexer)) (lExtensions Text.Highlighter.Lexers.Sql.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.SquidConf.lexer)) (lExtensions Text.Highlighter.Lexers.SquidConf.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Tcl.lexer)) (lExtensions Text.Highlighter.Lexers.Tcl.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Tcsh.lexer)) (lExtensions Text.Highlighter.Lexers.Tcsh.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Tex.lexer)) (lExtensions Text.Highlighter.Lexers.Tex.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Vala.lexer)) (lExtensions Text.Highlighter.Lexers.Vala.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.VbNet.lexer)) (lExtensions Text.Highlighter.Lexers.VbNet.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Velocity.lexer)) (lExtensions Text.Highlighter.Lexers.Velocity.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Verilog.lexer)) (lExtensions Text.Highlighter.Lexers.Verilog.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Vim.lexer)) (lExtensions Text.Highlighter.Lexers.Vim.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Xml.lexer)) (lExtensions Text.Highlighter.Lexers.Xml.lexer)
    , map (\e -> (e, Text.Highlighter.Lexers.Xslt.lexer)) (lExtensions Text.Highlighter.Lexers.Xslt.lexer)
    ]

