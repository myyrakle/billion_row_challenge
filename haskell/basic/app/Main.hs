module Main where

import Data.Int
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)

import System.CPUTime
import Conduit

import qualified Data.Text as T
import qualified Data.Map.Strict as Map

outputsPath = "outputs.txt"
measurementsPath = "measurements.txt"

data Status = Status {
    minVal :: Int64,
    maxVal :: Int64,
    total :: Int64,
    count :: Int64
} deriving (Show, Eq)


updateStatus :: Status -> Int64 -> Status
updateStatus (Status minVal maxVal total count) newVal = Status (minVal `min` newVal) (maxVal `max` newVal) (total + newVal) (count + 1)

solution :: IO String
solution = runConduitRes 
         $ sourceFile measurementsPath
        .| linesUnboundedAsciiC
        .| decodeUtf8C
        .| mapC (T.splitOn (T.pack ";"))
        .| foldlC (flip updateMap) (Map.empty :: Map T.Text Status)
        .| mapMC (sequence . fmap formatResult . Map.toList)
        .| sinkList
        >>= return . T.unpack . T.concat
    where
    updateMap :: [T.Text] -> Map T.Text Status -> Map T.Text Status
    updateMap [city, measurement] map = 
        let newVal = read (T.unpack measurement) :: Integer
            oldStatus = fromMaybe (Status (fromIntegral newVal) (fromIntegral newVal) 0 0) (Map.lookup city map)
        in Map.insert city (updateStatus oldStatus (fromIntegral newVal)) map
    formatResult :: (T.Text, Status) -> T.Text
    formatResult (city, status) = 
        let avg = total status `div` count status
        in T.concat [city, T.pack "=", T.pack (show (minVal status)), T.pack ";", T.pack (show (maxVal status)), T.pack ";", T.pack (show avg), T.pack "(", T.pack (show (total status)), T.pack "/", T.pack (show (count status)), T.pack ")\n"]

main = do
    expectOutput <- readFile outputsPath

    start <- getCPUTime
    got <- solution
    end <- getCPUTime

    let diff = floor((fromIntegral (end - start)) / (10^9)) :: Integer
    putStrLn $ "Elapsed: " ++ show diff ++ " ms"

    if got == expectOutput
        then putStrLn "Test Passed"
        else do 
            putStrLn "Test Failed"
            putStrLn $ "Expected: " ++ expectOutput
            putStrLn $ "Got: " ++ got