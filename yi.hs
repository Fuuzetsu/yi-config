import Yi
import Yi.Prelude
import Yi.Keymap.Emacs (keymap)
import Yi.UI.Pango (start)
import Yi.Prelude
import Yi.Style.Library
import Yi.Style
import Data.Monoid
import Data.Bits
import Data.Word

myConfig = defaultEmacsConfig

main :: IO ()
main = yi $ myConfig {
  defaultKm = defaultKm myConfig
  , startFrontEnd = start
  , configUI = (configUI defaultConfig) { configTheme = zenburnTheme }
  }


zenburnTheme = defaultTheme `override` \super _ -> super
  { modelineAttributes = emptyAttributes { foreground = notWhite, background = notBlack , reverseAttr = False }
  , modelineFocusStyle = withFg white `mappend` withBg black
  , baseAttributes     = emptyAttributes { foreground = notWhite, background = notBlack ,reverseAttr = False}
  , typeStyle          = withFg typ
  , selectedStyle      = withFg notWhite `mappend` withBg selectBG
  , eofStyle           = withFg notRed
  , hintStyle          = withFg notRed
  , strongHintStyle    = withFg notRed `mappend` withBd True
  , errorStyle         = withFg notBlack `mappend` withBg notRed
  , numberStyle        = withFg number
  , commentStyle       = withFg comment `mappend` withItlc True
  , keywordStyle       = withFg keyword `mappend` withBd True `mappend` withItlc True
  , variableStyle      = withFg variable
  , operatorStyle      = withFg operator
  , stringStyle        = withFg string
  , importStyle        = withFg statement
  , preprocessorStyle  = withFg preproc
  }

foo = "bar"

rgb :: Word32 -> Color
rgb x = RGB (fi (x `shiftR` 16))
            (fi (x `shiftR` 8))
            (fi x)
  where
    fi = fromIntegral

notBlack = rgb 0x3f3f3f
notWhite = rgb 0xdcdccc
keyword = rgb 0xf0dfaf
comment = rgb 0x7f9f7f
variable = rgb 0xefdcbc
operator = rgb 0xf0efd0
number = rgb 0x8cd0d3
typ = rgb 0xdfdfbf
notRed = rgb 0xcc7a00
define = rgb 0xdfcfaf
statement = rgb 0xe3ceab
preproc = rgb 0xffcfaf
selectBG = rgb 0x0f0f0f
string = rgb 0xcc9393
