import           Control.Applicative ((<|>))
import           Control.Lens ((%~))
import           Data.Bits
import           Yi
import           Yi.FuzzyOpen
import           Yi.Hoogle
import           Yi.Style.Monokai
import           Yi.UI.Pango (start)
import qualified Yi.Keymap.Emacs as Emacs
import qualified Yi.Mode.Haskell as Haskell
import           Yi.Mode.Haskell.Utils (ghciInsertMissingTypes,
                                        getTypeAtPoint, caseSplitAtPoint)
import           Yi.Mode.Haskell.Utils.PastePipe (lpasteCustom)
import           Yi.Monad (gets)

myModeTable :: [AnyMode]
myModeTable =
  [ AnyMode $ haskellModeHooks Haskell.preciseMode
  ] ++ modeTable defaultEmacsConfig


myKeymap :: KeymapSet
myKeymap = Emacs.mkKeymap $ override Emacs.defKeymap $ \proto _self ->
   proto {
           Emacs.eKeymap = Emacs.eKeymap proto
                           <|> (ctrlCh 'c' ?>> ctrlCh 'f' ?>>! fuzzyOpen)
         }

haskellModeHooks :: Mode syntax -> Mode syntax
haskellModeHooks mode =
  mode { modeKeymap =
            topKeymapA %~ ((ctrlCh 'c' ?>> choice cMaps) <||)
       }
  where
    cMaps = [ ctrlCh 'l' ?>>! ghciLoadBuffer
            , ctrlCh 'h' ?>> ctrlCh 't' ?>>! Haskell.ghciInferType
            , ctrlCh 'h' ?>> ctrlCh 'm' ?>>! ghciInsertMissingTypes
            , ctrlCh 'h' ?>> ctrlCh 'c' ?>>! getTypeAtPoint
            , ctrlCh 'h' ?>> ctrlCh 's' ?>>! caseSplitAtPoint
            , ctrlCh 'h' ?>> ctrlCh 'h' ?>>! hoogleSearch
            , ctrlCh 'h' ?>> ctrlCh 'p' ?>>! withBuffer (gets file) >>= \t ->
                lpasteCustom "Fūzetsu" t "haskell"
            ]

myConfig :: Config
myConfig = defaultEmacsConfig
  { defaultKm = myKeymap
  , modeTable = myModeTable
  }

main :: IO ()
main = yi $ myConfig {
  defaultKm = defaultKm myConfig
  , startFrontEnd = start
  , configUI = (configUI defaultConfig) { configTheme = monokaiTheme }
  }
