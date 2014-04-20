module Main where

import Test.QuickCheck
import Data.List
import Control.Monad(liftM)
import Data.List(intercalate)

import System.Environment

minSize' :: Int
minSize' = 2

maxSize' :: Int
maxSize' = 500

main :: IO ()
main = do
  putStrLn pre
  s <- liftM head $ sample' $ choose (minSize', maxSize')
  liftM head (sample' (vectorOf s (choose(0, 500)))) >>=
    putStrLn . showTC
  putStrLn post

showTC :: [Int] -> String
showTC vs = unlines
            [ "  int buf[" ++ show (length vs)
              ++ "] = {" ++ intercalate ", " (map show vs) ++ "};"
            , "  merge_sort(buf, " ++ show (length vs) ++ ");"
            , "  assert(is_sorted(buf, "  ++ show (length vs) ++ "));"
            ]

pre :: String
pre = unlines
      [ "#include <stdlib.h>"
      , "#include <assert.h>"
      , "#include \"merge_sort.h\""
      , ""
      , "int is_sorted (int buf[], int s) {"
      , "  int i;"
      , "  for (i = 0; i < s - 1; i++) {"
      , "  if (buf[i] > buf[i+1])"
      , "    return 0;"
      , "  }"
      , "  return 1;"
      , "}"
      , ""
      , "int test_main (void) {"
      ]

post :: String
post = unlines
       [ "  return 0;"
       , "}"
       ]
