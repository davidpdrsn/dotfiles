import XMonad

main = do
  xmonad $ defaultConfig { borderWidth = 2
                         , normalBorderColor = "#abc123"
                         , focusedBorderColor = "#456def"
                         , terminal = "urxvt"
                         }
