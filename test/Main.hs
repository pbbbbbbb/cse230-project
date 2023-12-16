import UnitTest

main :: IO ()
main = do
   res <- testEnemyList 500
   if res
       then putStrLn "Test passed."
       else putStrLn "Test failed."
