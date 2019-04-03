module ResultChecker where

import Elgamal

type Vote = (String,[Cipher])

type Register = [Vote]


parseVotes :: [String] -> Vote
parseVotes (addr:votes) = (addr:)
    where
        

--readElectoralRegister :: String -> IO Register


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

