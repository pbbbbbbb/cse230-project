module Main (main) where

import UI (gameInit)
-- import UnitTest

main :: IO ()
main = do
  g <- gameInit
  return ()

--main for UnitTest
--main :: IO ()
--main = do
--  res <- testEnemyList 500
--  if res
--      then putStrLn "True"
--      else putStrLn "False"
