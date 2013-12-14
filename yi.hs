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
  [ AnyMode Haskell.preciseMode
  ] ++ modeTable defaultEmacsConfig

myConfig = defaultEmacsConfig
  { defaultKm =
       Emacs.mkKeymap $ override Emacs.defKeymap $ \parent _ ->
         parent { Emacs.eKeymap =
                     Emacs.eKeymap parent ||> (ctrlCh 'c' ?>> ctrlCh 'l'
                                               ?>>! ghciLoadBuffer)
                }
  , modeTable = myModeTable
  }

main :: IO ()
main = yi $ myConfig {
  defaultKm = defaultKm myConfig
  , startFrontEnd = start
  , configUI = (configUI defaultConfig) { configTheme = monokaiTheme }
  }
