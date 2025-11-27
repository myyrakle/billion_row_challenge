{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (sortBy)
import Data.Ord (comparing)
import System.IO
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- 경로 상수
outputPath :: FilePath
outputPath = "outputs.txt"

measurementsPath :: FilePath
measurementsPath = "measurements.txt"

-- 통계 데이터 구조
data Status = Status
    { sMin   :: !Int
    , sMax   :: !Int
    , sTotal :: !Int
    , sCount :: !Int
    } deriving (Show)

-- 수동 정수 파싱
parseIntManual :: T.Text -> Maybe Int
parseIntManual txt = go 0 0
  where
    len = T.length txt
    go !acc !i
      | i >= len = Just acc
      | otherwise =
          let c = T.index txt i
              digit = fromEnum c - fromEnum '0'
          in if digit >= 0 && digit <= 9
             then go (acc * 10 + digit) (i + 1)
             else Nothing

-- 라인 파싱 함수 (Text 사용)
parseLine :: T.Text -> Maybe (T.Text, Int)
parseLine line =
    case T.breakOn ";" line of
        (city, rest) | not (T.null rest) ->
            parseIntManual (T.tail rest) >>= \num -> Just (city, num)
        _ -> Nothing

-- Status 업데이트 함수 (strict)
updateStatus :: Status -> Int -> Status
updateStatus !status !measurement =
    Status
        { sMin = min (sMin status) measurement
        , sMax = max (sMax status) measurement
        , sTotal = sTotal status + measurement
        , sCount = sCount status + 1
        }

-- 새로운 Status 생성
newStatus :: Int -> Status
newStatus !measurement =
    Status
        { sMin = measurement
        , sMax = measurement
        , sTotal = measurement
        , sCount = 1
        }

-- 라인 처리 함수 (꼬리 재귀로 메모리 최적화)
processLines :: Handle -> Map.Map T.Text Status -> IO (Map.Map T.Text Status)
processLines h !acc = do
    eof <- hIsEOF h
    if eof
        then return acc
        else do
            line <- TIO.hGetLine h
            let !newAcc = case parseLine line of
                    Just (city, measurement) ->
                        Map.insertWith (\_ old -> updateStatus old measurement)
                                     city
                                     (newStatus measurement)
                                     acc
                    Nothing -> acc
            processLines h newAcc

-- 포맷팅 함수
formatOutput :: (T.Text, Status) -> String
formatOutput (city, status) =
    let avg = sTotal status `div` sCount status
    in T.unpack city ++ "=" ++
       show (sMin status) ++ ";" ++
       show (sMax status) ++ ";" ++
       show avg ++ "(" ++
       show (sTotal status) ++ "/" ++
       show (sCount status) ++ ")\n"

-- 메인 솔루션 함수
solution :: FilePath -> IO String
solution path = do
    withFile path ReadMode $ \h -> do
        hSetEncoding h utf8
        !statsMap <- processLines h Map.empty

        let sortedList = sortBy (comparing fst) $ Map.toList statsMap
            result = concatMap formatOutput sortedList

        return result

-- 메인 함수
main :: IO ()
main = do
    expectedOutput <- readFile outputPath

    startTime <- getCurrentTime
    result <- solution measurementsPath
    let !_ = length result  -- Force evaluation
    endTime <- getCurrentTime

    let elapsed = diffUTCTime endTime startTime
        elapsedMs = realToFrac elapsed * 1000 :: Double

    putStrLn $ "Elapsed: " ++ show (round elapsedMs :: Int) ++ "ms"

    if expectedOutput == result
        then putStrLn "Matched!"
        else error "Output does not match expected!"
