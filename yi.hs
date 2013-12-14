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
  , configUI = (configUI defaultConfig) { configTheme = monokaiTheme }
  }
