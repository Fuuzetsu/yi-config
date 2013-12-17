import           Data.Bits
import           Yi hiding (foldl, (.), notElem, mapM, mapM_)
import           Yi.Style.Monokai
import           Yi.UI.Pango (start)
import qualified Yi.Keymap.Emacs as Emacs
import qualified Yi.Mode.Haskell as Haskell
import           Yi.Mode.Haskell.Utils (ghciInsertMissingTypes)

myModeTable :: [AnyMode]
myModeTable =
  [ AnyMode $ haskellModeHooks Haskell.cleverMode
  ] ++ modeTable defaultEmacsConfig

haskellModeHooks :: Mode syntax -> Mode syntax
haskellModeHooks mode =
  mode { modeKeymap =
            topKeymapA ^: ((ctrlCh 'c' ?>> choice cMaps) <||)
       }
  where
    cMaps = [ ctrlCh 'l' ?>>! ghciLoadBuffer
            , ctrlCh 'h' ?>> ctrlCh 't' ?>>! Haskell.ghciInferType
            , ctrlCh 'h' ?>> ctrlCh 'm' ?>>! ghciInsertMissingTypes
            ]

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
