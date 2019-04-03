module ResultChecker where

import Elgamal
import EllipticAlgebra
import Data.List.Split

type Vote = (String,[Cipher])

type Register = [Vote]

--             p       c      rawInfo
parseVoters :: Int -> Int -> [String] -> Register
parseVoters p c raw = map (parseVoter c)(chunksOf (c+1) raw) 


parseRegister :: String -> Register
parseRegister raw = parseVoters p c (drop 2 rawlines)
    where
        rawlines = lines raw :: [String]
        p = read (rawlines !! 0) :: Int
        c = read (rawlines !! 1) :: Int

--      nCandidates  rawvotes    (parsedVote)
parseVoter :: Int -> [String] -> Vote
parseVoter n (addr:rawVotes) = (addr,rawToCiphers rawCiphers)
    where
        rawCiphers = map (map read) (map words rawVotes) :: [[Integer]]

-- raw ciphers to cipher
rawToCiphers :: [[Integer]] -> [Cipher]
rawToCiphers [] = []
rawToCiphers (rawVote:rawVotes) =  ((Coord (rawVote !! 0) (rawVote !! 1) ),(Coord (rawVote !! 2) (rawVote !! 3))): rawToCiphers rawVotes

readElectoralRegister :: String -> IO Register
readElectoralRegister path = do
    rawFile <- readFile path
    return $ parseRegister rawFile


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

{-
["MyAddr","6 303 343 283","7 301 219 229","305 249 199 296","MyAddr","6 303 343 283","7 301 219 229","305 249 199 296","MyAddr","6 303 343 283","7 301 219 229","305 249 199 296","MyAddr","6 303 343 283","7 301 219 229","305 249 199 296"]
-}