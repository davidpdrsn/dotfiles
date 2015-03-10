{-# LANGUAGE
     DeriveDataTypeable,
     FlexibleContexts,
     FlexibleInstances,
     MultiParamTypeClasses,
     NoMonomorphismRestriction,
     PatternGuards,
     ScopedTypeVariables,
     TypeSynonymInstances,
     UndecidableInstances
     #-}

import Control.Monad
import Data.List
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import System.IO
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.Run

main :: IO ()
main = do
  spawn "feh --bg-scale ~/background.png"
  spawn "xbindkeys"
  spawn "prevent-death"
  spawn "dunst"
  spawn "xscreensaver -no-splash"
  spawn "xss-lock -- xscreensaver-command -lock"
  xmproc <- spawnPipe "xmobar"

  checkTopicConfig myTopics myTopicConfig

  xmonad $ defaultConfig { borderWidth = 2
                         , normalBorderColor =  "#111111"
                         , focusedBorderColor = "#333333"
                         , workspaces = myTopics
                         , terminal = "urxvt"

                         , manageHook = manageDocks <+> manageHook defaultConfig
                         , layoutHook = avoidStruts $ layoutHook defaultConfig
                         , logHook = dynamicLogWithPP xmobarPP
                                         { ppOutput = hPutStrLn xmproc
                                         , ppTitle = xmobarColor "green" "" . shorten 50
                                         }
                         } `additionalKeys` myKeys

--------------------------------------------------------------
-------------------- Keys ------------------------------------
--------------------------------------------------------------

modm :: KeyMask
modm = mod1Mask

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [ ((modm, xK_g), warpToCentre >> promptedGoto)
         , ((modm, xK_c), spawn "chromium")
         ]

warpToCentre :: X ()
warpToCentre = gets (W.screen . W.current . windowset) >>= \x -> warpToScreen x  0.5 0.5

--------------------------------------------------------------
-------------------- Topics ----------------------------------
--------------------------------------------------------------

myTopics :: [Topic]
myTopics =
  [ "misc"
  , "dotfiles"
  , "osm"
  , "alp"
  , "tonsser"
  , "web"
  ] ++ map show ([1..8] :: [Int])

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
  { topicDirs = M.fromList $
      [ ("misc", "~/")
      , ("dotfiles", "~/dotfiles")
      , ("osm", "~/uni/osm")
      , ("alp", "~/uni/alp")
      , ("tonsser", "~/projects/tonsser-api")
      ]
    , defaultTopicAction = const $ spawnShell >*> 1
    , defaultTopic = "dotfiles"
    , maxTopicHistory = 10
    , topicActions = M.fromList $
        [ ("web", spawnOn "web" (intercalate " " $ "chromium" : openingSites))
        ]
  }

-- Sites I want to open in my web topic
openingSites :: [String]
openingSites = [ "gmail.com"
               , "icloud.com"
               , "github.com"
               , "youtube.com"
               , "facebook.com"
               ] |> map ("http://"++)

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: [Char] -> X ()
spawnShellIn dir = do
    t <- asks (terminal . config)
    spawnHere $ "cd " ++ dir ++ " && " ++ t

gsConfig :: HasColorizer a => GSConfig a
gsConfig = defaultGSConfig

wsgrid :: X (Maybe String)
wsgrid = gridselect gsConfig <=< asks $ map (\x -> (x,x)) . workspaces . config

promptedGoto :: X ()
promptedGoto = wsgrid >>= flip whenJust (switchTopic myTopicConfig)

--------------------------------------------------------------
-------------------- Misc helper things ----------------------
--------------------------------------------------------------

-- Pipeline operator shamelessly stolen from Elixir
(|>) :: a -> (a -> b) -> b
x |> f = f x
