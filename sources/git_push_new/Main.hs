module Main (main) where

import System.Process
import System.IO
import Data.Char (isSpace)
import Data.List

main :: IO ()
main = do
    (_, Just out, _, _) <- createProcess (shell "git push 2>&1") { std_out = CreatePipe }
    output <- hGetContents out
    let pushCmd = words . trim . last . filter (/= "") . lines $ output
    if pushCmd == ["Everything","up-to-date"]
      then putStrLn $ intercalate " " pushCmd
      else if "->" `elem` pushCmd
             then putStrLn output
             else readProcess (head pushCmd) (tail pushCmd) "" >> return ()

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
