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
        .| linesUnboundedAscii
        .| mapC (T.splitOn (T.pack ";"))
        .| foldlC updateMap Map.empty
        .| mapC formatResult
        .| sinkList
        >>= return . T.unpack . T.concat
    where
    updateMap :: Map T.Text Status -> [T.Text] -> Map T.Text Status
    updateMap map [city, measurement] = 
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