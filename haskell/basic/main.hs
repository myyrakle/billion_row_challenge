module Main where

import Data.Int
import Data.List
import Data.Function (on)
import System.CPUTime
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Control.DeepSeq (deepseq)

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
solution = do
    content <- TLIO.readFile measurementsPath
    let lines = TL.lines content
    let cityMap = foldl updateMap Map.empty lines
    let results = sortBy (compare `on` fst) $ Map.toList cityMap
    return $ TL.unpack $ TL.concat $ map formatResult results
    where
    updateMap :: Map T.Text Status -> TL.Text -> Map T.Text Status
    updateMap map line = 
        let [city, measurement] = TL.splitOn (TL.pack ";") line
            newVal = read (TL.unpack measurement) :: Integer
            oldStatus = fromMaybe (Status (fromIntegral newVal) (fromIntegral newVal) 0 0) (Map.lookup (TL.toStrict city) map)
        in Map.insert (TL.toStrict city) (updateStatus oldStatus (fromIntegral newVal)) map
    formatResult :: (T.Text, Status) -> TL.Text
    formatResult (city, status) = 
        let avg = total status `div` count status
        in TL.concat [TL.fromStrict city, TL.pack "=", TL.pack (show (minVal status)), TL.pack ";", TL.pack (show (maxVal status)), TL.pack ";", TL.pack (show avg), TL.pack "(", TL.pack (show (total status)), TL.pack "/", TL.pack (show (count status)), TL.pack ")\n"]

main = do
    expectOutput <- readFile outputsPath

    start <- getCPUTime
    got <- solution
    got `deepseq` return ()
    end <- getCPUTime

    let diff = floor((fromIntegral (end - start)) / (10^9)) :: Integer
    putStrLn $ "Elapsed: " ++ show diff ++ " ms"

    if got == expectOutput
        then putStrLn "Test Passed"
        else do 
            putStrLn "Test Failed"
            putStrLn $ "Expected: " ++ expectOutput
            putStrLn $ "Got: " ++ got