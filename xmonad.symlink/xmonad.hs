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
  xmproc <- spawnPipe "xmobar"

  checkTopicConfig myTopics myTopicConfig

  xmonad $ defaultConfig { borderWidth = 2
                         , normalBorderColor =  "#111111"
                         , focusedBorderColor = "#333333"
                         , workspaces = myTopics
                         , terminal = myTerminal

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

-- Sites I want to open in my web topic
toUrls :: [String] -> [String]
toUrls = map ("http://"++)

myTopics :: [Topic]
myTopics =
  [ "home"
  , "dotfiles"
  , "osm"
  , "alp"
  , "tonsser"
  , "web"
  , "procrastination"
  , "irc"
  , "organize"
  , "2. dan"
  ]

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
  { topicDirs = M.fromList $
      [ ("home", "~/")
      , ("dotfiles", "~/dotfiles")
      , ("osm", "~/uni/osm")
      , ("alp", "~/uni/alp")
      , ("tonsser", "~/projects/tonsser-api")
      ]
    , defaultTopicAction = const $ spawnShell >*> 1
    , defaultTopic = "dotfiles"
    , maxTopicHistory = 10
    , topicActions = M.fromList $
          [ ("web", openInBrowser [ "about:blank"
                                  ])
          , ("procrastination", openInBrowser [ "facebook.com"
                                              , "twitter.com"
                                              , "9gag.com"
                                              , "youtube.com"
                                              , "github.com"
                                              , "upcase.com"
                                              ])
          , ("organize", openInBrowser [ "gmail.com"
                                       , "icloud.com"
                                       ])
          , ("alp", runAllCmd [ asPdf "~/uni/alp/proglang.pdf"
                              , asPdf "~/uni/alp/g-assignment/Assignment3-2015.pdf"
                              ])
          , ("osm", runAllCmd [ asPdf "~/uni/osm/roadmap.pdf"
                              , asPdf "~/uni/osm/group/g5/g5.pdf"
                              , inTerm "tmuxinator osm"
                              ])
          , ("tonsser", runCmd $ inTerm "tmuxinator tonsser")
          , ("dotfiles", runCmd $ inTerm "tmuxinator dotfiles")
          ]
  }

openInBrowser :: [String] -> X ()
openInBrowser ss = spawn $ (intercalate " " $ (:) "chromium --new-window " $ toUrls ss)

data TermCmd = TermCmd { termCmd :: String }

myTerminal :: String
myTerminal = "urxvt"

runCmd :: TermCmd -> X ()
runCmd = spawn . termCmd

inTerm :: String -> TermCmd
inTerm cmd = TermCmd $ myTerminal ++ " -e " ++ cmd

runAllCmd :: [TermCmd] -> X ()
runAllCmd = spawn . intercalate "; " . map termCmd

asPdf :: String -> TermCmd
asPdf s = TermCmd $ "zathura --fork " ++ s ++ " > /dev/null 2>&1"

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
