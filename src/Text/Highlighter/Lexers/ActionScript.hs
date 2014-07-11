module Text.Highlighter.Lexers.ActionScript (lexer) where

import Text.Regex.PCRE.Light
import Text.Highlighter.Types

lexer :: Lexer
lexer = Lexer
    { lName = "ActionScript"
    , lAliases = ["as", "actionscript"]
    , lExtensions = [".as"]
    , lMimetypes = ["application/x-actionscript", "text/x-actionscript", "text/actionscript"]
    , lStart = root'
    , lFlags = [dotall]
    }

root' :: TokenMatcher
root' =
    [ tok "\\s+" (Arbitrary "Text")
    , tok "//.*?\\n" (Arbitrary "Comment" :. Arbitrary "Single")
    , tok "/\\*.*?\\*/" (Arbitrary "Comment" :. Arbitrary "Multiline")
    , tok "/(\\\\\\\\|\\\\/|[^/\\n])*/[gim]*" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Regex")
    , tok "[\126\\^\\*!%&<>\\|+=:;,/?\\\\-]+" (Arbitrary "Operator")
    , tok "[{}\\[\\]();.]+" (Arbitrary "Punctuation")
    , tok "(case|default|for|each|in|while|do|break|return|continue|if|else|throw|try|catch|var|with|new|typeof|arguments|instanceof|this|switch)\\b" (Arbitrary "Keyword")
    , tok "(class|public|final|internal|native|override|private|protected|static|import|extends|implements|interface|intrinsic|return|super|dynamic|function|const|get|namespace|package|set)\\b" (Arbitrary "Keyword" :. Arbitrary "Declaration")
    , tok "(true|false|null|NaN|Infinity|-Infinity|undefined|Void)\\b" (Arbitrary "Keyword" :. Arbitrary "Constant")
    , tok "(Accessibility|AccessibilityProperties|ActionScriptVersion|ActivityEvent|AntiAliasType|ApplicationDomain|AsBroadcaster|Array|AsyncErrorEvent|AVM1Movie|BevelFilter|Bitmap|BitmapData|BitmapDataChannel|BitmapFilter|BitmapFilterQuality|BitmapFilterType|BlendMode|BlurFilter|Boolean|ByteArray|Camera|Capabilities|CapsStyle|Class|Color|ColorMatrixFilter|ColorTransform|ContextMenu|ContextMenuBuiltInItems|ContextMenuEvent|ContextMenuItem|ConvultionFilter|CSMSettings|DataEvent|Date|DefinitionError|DeleteObjectSample|Dictionary|DisplacmentMapFilter|DisplayObject|DisplacmentMapFilterMode|DisplayObjectContainer|DropShadowFilter|Endian|EOFError|Error|ErrorEvent|EvalError|Event|EventDispatcher|EventPhase|ExternalInterface|FileFilter|FileReference|FileReferenceList|FocusDirection|FocusEvent|Font|FontStyle|FontType|FrameLabel|FullScreenEvent|Function|GlowFilter|GradientBevelFilter|GradientGlowFilter|GradientType|Graphics|GridFitType|HTTPStatusEvent|IBitmapDrawable|ID3Info|IDataInput|IDataOutput|IDynamicPropertyOutputIDynamicPropertyWriter|IEventDispatcher|IExternalizable|IllegalOperationError|IME|IMEConversionMode|IMEEvent|int|InteractiveObject|InterpolationMethod|InvalidSWFError|InvokeEvent|IOError|IOErrorEvent|JointStyle|Key|Keyboard|KeyboardEvent|KeyLocation|LineScaleMode|Loader|LoaderContext|LoaderInfo|LoadVars|LocalConnection|Locale|Math|Matrix|MemoryError|Microphone|MorphShape|Mouse|MouseEvent|MovieClip|MovieClipLoader|Namespace|NetConnection|NetStatusEvent|NetStream|NewObjectSample|Number|Object|ObjectEncoding|PixelSnapping|Point|PrintJob|PrintJobOptions|PrintJobOrientation|ProgressEvent|Proxy|QName|RangeError|Rectangle|ReferenceError|RegExp|Responder|Sample|Scene|ScriptTimeoutError|Security|SecurityDomain|SecurityError|SecurityErrorEvent|SecurityPanel|Selection|Shape|SharedObject|SharedObjectFlushStatus|SimpleButton|Socket|Sound|SoundChannel|SoundLoaderContext|SoundMixer|SoundTransform|SpreadMethod|Sprite|StackFrame|StackOverflowError|Stage|StageAlign|StageDisplayState|StageQuality|StageScaleMode|StaticText|StatusEvent|String|StyleSheet|SWFVersion|SyncEvent|SyntaxError|System|TextColorType|TextField|TextFieldAutoSize|TextFieldType|TextFormat|TextFormatAlign|TextLineMetrics|TextRenderer|TextSnapshot|Timer|TimerEvent|Transform|TypeError|uint|URIError|URLLoader|URLLoaderDataFormat|URLRequest|URLRequestHeader|URLRequestMethod|URLStream|URLVariabeles|VerifyError|Video|XML|XMLDocument|XMLList|XMLNode|XMLNodeType|XMLSocket|XMLUI)\\b" (Arbitrary "Name" :. Arbitrary "Builtin")
    , tok "(decodeURI|decodeURIComponent|encodeURI|escape|eval|isFinite|isNaN|isXMLName|clearInterval|fscommand|getTimer|getURL|getVersion|isFinite|parseFloat|parseInt|setInterval|trace|updateAfterEvent|unescape)\\b" (Arbitrary "Name" :. Arbitrary "Function")
    , tok "[$a-zA-Z_][a-zA-Z0-9_]*" (Arbitrary "Name" :. Arbitrary "Other")
    , tok "[0-9][0-9]*\\.[0-9]+([eE][0-9]+)?[fd]?" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Float")
    , tok "0x[0-9a-f]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Hex")
    , tok "[0-9]+" (Arbitrary "Literal" :. Arbitrary "Number" :. Arbitrary "Integer")
    , tok "\"(\\\\\\\\|\\\\\"|[^\"])*\"" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Double")
    , tok "'(\\\\\\\\|\\\\'|[^'])*'" (Arbitrary "Literal" :. Arbitrary "String" :. Arbitrary "Single")
    ]

