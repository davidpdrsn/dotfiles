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

import Control.Applicative
import Control.Monad
import Control.Monad.Instances ()
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Data.Traversable(traverse)
import Graphics.X11.Xinerama
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import System.IO
import XMonad
import XMonad.Actions.DwmPromote
import XMonad.Actions.FloatSnap
import XMonad.Actions.GridSelect
import XMonad.Actions.Search
import XMonad.Actions.SpawnOn
import XMonad.Actions.Submap
import XMonad.Actions.TopicSpace
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BoringWindows
import XMonad.Layout.Drawer
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Magnifier
import XMonad.Layout.Master
import XMonad.Layout.Mosaic
import XMonad.Layout.MosaicAlt
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.TrackFloating
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Replace
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

myFont :: String
myFont = "xft:Inconsolata:size=12"

--------------------------------------------------------------
-------------------- GridSelect ------------------------------
--------------------------------------------------------------

myGridSelectConfig :: HasColorizer a => GSConfig a
myGridSelectConfig = defaultGSConfig { gs_cellheight  = 35
                                     , gs_cellwidth   = 100
                                     -- , gs_navigate    = M.unions [ reset, fpsKeys ]
                                     , gs_font        = myFont
                                     , gs_cellpadding = 4
                                     }
                                     --
--------------------------------------------------------------
-------------------- Keys ------------------------------------
--------------------------------------------------------------

modm :: KeyMask
modm = mod1Mask

myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [ ((modm, xK_g), warpToCentre >> promptedGoto)
         , ((modm, xK_c), spawn "chromium")
         ]

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
  ] ++ map show [1..8]

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

spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn dir = do
    t <- asks (terminal . config)
    spawnHere $ "cd " ++ dir ++ " && " ++ t

gsConfig = defaultGSConfig

wsgrid = gridselect gsConfig <=< asks $ map (\x -> (x,x)) . workspaces . config

promptedGoto = wsgrid >>= flip whenJust (switchTopic myTopicConfig)

-- Moves the selection into current topic
promptedShift = wsgrid >>= \x -> whenJust x $ \y -> windows (W.greedyView y . W.shift y)

--------------------------------------------------------------
-------------------- Misc helper things ----------------------
--------------------------------------------------------------

-- Pipeline operator shamelessly stolen from Elixir
(|>) :: a -> (a -> b) -> b
x |> f = f x
