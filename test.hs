import System.Random (randomR, newStdGen, StdGen)

randomInt :: Int -> Int -> IO Int
randomInt lo hi = do
    gen <- newStdGen
    let (value, _) = randomR (lo, hi) gen
    return value


-- >>> randomInt 1 10
-- (Error while loading modules for evaluation)
-- <BLANKLINE>
-- <no location info>: warning: [-Wmissing-home-modules]
--     Modules are not listed in command line but needed for compilation:
--         Bullet
-- [4 of 6] Compiling Player           ( /Users/risulet/Documents/cse230-project/Player.hs, interpreted )
-- <BLANKLINE>
-- /Users/risulet/Documents/cse230-project/Player.hs:31:1-6: error:
--     parse error on input ‘import’
-- Failed, three modules loaded.
--
