import XMonad
import XMonad.Util.EZConfig

main :: IO ()
main = do
  spawn "xbindkeys"
  spawn "prevent-death"
  spawn "dunst"

  xmonad $ def { borderWidth = 2
               , normalBorderColor = "#111111"
               , focusedBorderColor = "#333333"
               , terminal = "urxvt"
               } `additionalKeys` myKeys

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [ ((mod1Mask, xK_b), spawn "notify-send \"`acpi`\"")
         , ((mod1Mask, xK_d), spawn "notify-send \"`date`\"")
         ]
