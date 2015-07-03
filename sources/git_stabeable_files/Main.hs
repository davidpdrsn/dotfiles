module Main (main) where

import qualified Data.List as L
import System.Process
import System.IO

main :: IO ()
main = do
    (_, Just out, _, _) <- createProcess (shell "git status -sb") { std_out = CreatePipe }
    output <- stagableFiles . lines <$> hGetContents out
    mapM_ putStrLn output

stagableFiles :: [String] -> [String]
stagableFiles = map clean . filter isStageable

clean :: String -> String
clean = drop 3

isStageable :: String -> Bool
isStageable s = isNotBranch s && (isNew s || hasUnstagedChanged s)
  where
    isNotBranch = not . L.isPrefixOf "##"
    isNew = L.isPrefixOf "??"
    hasUnstagedChanged = (' ' /=) . (!! 1)
