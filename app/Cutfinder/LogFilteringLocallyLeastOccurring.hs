module Cutfinder.LogFilteringLocallyLeastOccurring (
    logFiltering
) where

{-
This implementation is based on:
Sander J. J. Leemans. Robust Process Mining with Guarantees - Process Discovery, Conformance Checking and Enhancement, volume 440 of Lecture Notes in Business Information Processing. Springer, 2022. ISBN 978-3-030-96654-6. doi: 10.1007/ 978-3-030-96655-3. https://doi.org/10.1007/978-3-030-96655-3.
-}

import Data.List
import Helperfunctions.HelperFunctions
import Data.List.Split


logFiltering :: Float -> [(Int, [String])] -> [(Int, [String])]
logFiltering f log = map (\(occ,x) -> (occ, traceFiltering f x)) log
  where
    traceFiltering :: Float -> [String] -> [String]
    traceFiltering f trace = 
      let transitions = sumValuesByPairs (getTransitions trace)
          getMaxOccurrence = maxOccurrence 0 transitions
          filteredTransitions = 
            transitionFiltering transitions ((intToFloat getMaxOccurrence) * f)
      in activitiesFiltering trace filteredTransitions

getTransitions :: [String] -> [(Int, (String, String))]
getTransitions [] = []
getTransitions [a] = []
getTransitions (a1:a2:as) = (1,(a1,a2)) : getTransitions (a2:as)

maxOccurrence :: Int -> [(Int, (String, String))] -> Int
maxOccurrence res [] = res
maxOccurrence res ((occ, (_, _)):xs) | occ > res = maxOccurrence occ xs
                                     | otherwise = maxOccurrence res xs

transitionFiltering :: [(Int, (String, String))] -> Float -> [(String, String)]
transitionFiltering [] _ = []
transitionFiltering ((occ,(x1,x2)):xs) relativeMaxOcc
  | (intToFloat occ) >= relativeMaxOcc = (x1,x2) : transitionFiltering xs relativeMaxOcc
  | otherwise = transitionFiltering xs relativeMaxOcc

activitiesFiltering :: [String] -> [(String, String)] -> [String]
activitiesFiltering [] _ = []
activitiesFiltering [a] _ = [a]
activitiesFiltering (a1:a2:as) transitions
  | elem (a1,a2) transitions = a1 : activitiesFiltering (a2:as) transitions
  | otherwise = activitiesFiltering (a1:as) transitions

sumValuesByTransitions :: [(Int, (String, String))] -> [(Int, (String, String))]
sumValuesByTransitions [] = []
sumValuesByTransitions (trace:traces) = sumValuesByTransitions trace traces [] []
  where
    sumValuesByTransitions :: (Int, (String, String)) -> [(Int, (String, String))] -> [(Int, (String, String))] -> [(Int, (String, String))] -> [(Int, (String, String))]
    sumValuesByTransitions (xOcc, xTrace) [] [] result = ((xOcc, xTrace):result)
    sumValuesByTransitions (xOcc, xTrace) [] (r:remaining) result = sumValuesByTransitions r remaining [] ((xOcc, xTrace):result)
    sumValuesByTransitions (xOcc, xTrace) ((yOcc, yTrace):ys) remaining result
      | xTrace == yTrace = sumValuesByTransitions (xOcc + yOcc, xTrace) ys remaining result
      | otherwise = sumValuesByTransitions (xOcc, xTrace) ys ((yOcc, yTrace):remaining) result
