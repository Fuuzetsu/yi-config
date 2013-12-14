import           Data.Bits
import           Data.Monoid
import           Data.Word
import           Yi
import           Yi.Prelude
import           Yi.Style
import           Yi.Style.Library
import           Yi.Style.Monokai
import           Yi.UI.Pango (start)
import qualified Yi.Keymap.Emacs as Emacs
import qualified Yi.Mode.Haskell as Haskell

myModeTable :: [AnyMode]
myModeTable =
  [ AnyMode $ haskellModeHooks Haskell.preciseMode
  ] ++ modeTable defaultEmacsConfig

haskellModeHooks :: Mode syntax -> Mode syntax
haskellModeHooks mode =
  mode { modeKeymap =
            topKeymapA ^: ((ctrlCh 'c' ?>> ctrlCh 'l' ?>>! ghciLoadBuffer) <||)
       }

myConfig :: Config
myConfig = defaultEmacsConfig
  { defaultKm = Emacs.mkKeymap Emacs.defKeymap
  , modeTable = myModeTable
  }

main :: IO ()
main = yi $ myConfig {
  defaultKm = defaultKm myConfig
  , startFrontEnd = start
  , configUI = (configUI defaultConfig) { configTheme = monokaiTheme }
  }
