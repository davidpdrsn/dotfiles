import XMonad

main :: IO ()
main = do
  spawn "xbindkeys"
  spawn "prevent-death"
  spawn "dunst"
  spawn "xset -b"

  xmonad $ defaultConfig { borderWidth = 2
                         , normalBorderColor = "#111111"
                         , focusedBorderColor = "#333333"
                         , terminal = "urxvt"
                         }
