import XMonad

main = do
  xmonad $ defaultConfig { borderWidth = 2
                         , normalBorderColor = "#111111"
                         , focusedBorderColor = "#333333"
                         , terminal = "urxvt"
                         }
