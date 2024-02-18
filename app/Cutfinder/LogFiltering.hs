module Cutfinder.LogFiltering (
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
logFiltering f log = 
  let transitions = transitionsEventLog log
      allAct = allActivities log
      endActOccurrences = sumValuesByTraces (map (\(occ, trace) -> (occ, [last trace])) log)
      filteredTransitions = transitionsFiltering f allAct endActOccurrences transitions
      frequentStartAct_ = frequentStartAct f log
      frequentEndAct_ = frequentEndAct f log

      logWithoutInfrequentStartAct = getFilteredLogStartAct frequentStartAct_ log
      logWithoutInfrequentTransitions = sumValuesByTraces (map (\(occ, trace) -> (occ, activitiesFiltering trace filteredTransitions)) logWithoutInfrequentStartAct)
      logWithoutInfrequentEndAct = getFilteredLogEndAct frequentEndAct_ logWithoutInfrequentTransitions
  in logWithoutInfrequentEndAct 
    where
      transitionsEventLog :: [(Int, [String])] -> [(Int, (String, String))]
      transitionsEventLog log = sumValuesByTransitions (concatMap transitionsLogHelper log)
        where
          transitionsLogHelper :: (Int,[String]) -> [(Int, (String, String))]
          transitionsLogHelper (_,[]) = []
          transitionsLogHelper (_,[_]) = []
          transitionsLogHelper (occ, (x:y:xs)) = (occ, (x, y)) : transitionsLogHelper (occ, (y:xs)) --the function includes self-loops

getFilteredLogStartAct :: [String] -> [(Int, [String])] -> [(Int, [String])]
getFilteredLogStartAct startAct log = 
  let logCorrectStart = map (\(occ, trace) -> (occ, replaceEmptyListWithTau (correctStartTrace trace startAct))) log
  in logCorrectStart
    where
      correctStartTrace :: [String] -> [String] -> [String]
      correctStartTrace [tau] startAct = [tau]
      correctStartTrace trace startAct
        | traceCorrectStart = trace
        | otherwise = correctStartTrace (init trace) startAct
            where 
              traceCorrectStart = elem (errorSafeHead trace) startAct

getFilteredLogEndAct :: [String] -> [(Int, [String])] -> [(Int, [String])]
getFilteredLogEndAct endAct log = 
  let logCorrectStart = map (\(occ, trace) -> (occ, replaceEmptyListWithTau (correctEndTrace trace endAct))) log
  in logCorrectStart
    where
      correctEndTrace :: [String] -> [String] -> [String]
      correctEndTrace [tau] endAct = [tau]
      correctEndTrace trace endAct
        | traceCorrectEnd = trace
        | otherwise = correctEndTrace (errorSafeTail trace) endAct
            where 
              traceCorrectEnd = elem (last trace) endAct

activitiesFiltering :: [String] -> [(String, String)] -> [String]
activitiesFiltering [] _ = []
activitiesFiltering [a] _ = [a]
activitiesFiltering (a1:a2:as) transitions
  | elem (a1,a2) transitions = a1 : activitiesFiltering (a2:as) transitions
  | otherwise = activitiesFiltering (a1:as) transitions

transitionsFiltering :: Float -> [String] -> [(Int, [String])] -> [(Int, (String, String))] -> [(String, String)]
transitionsFiltering f allAct endActOccurrences transitions = concatMap (\x -> transitionsFilteringPerAct f x endActOccurrences transitions) allAct
  where
    transitionsFilteringPerAct :: Float -> String -> [(Int, [String])] -> [(Int, (String, String))] -> [(String, String)]
    transitionsFilteringPerAct f currentAct endActOccurrences transitions = 
      let outgoingTransitionsOfCurrentAct = getOutgoingTransitionsOfCurrentAct currentAct transitions
          actOccurrenceAsEndAct = getOccurrenceOfCurrentAct currentAct endActOccurrences
          maxOccurrenceOfCurrentAct = max (getMaxOccurrence 0 outgoingTransitionsOfCurrentAct) actOccurrenceAsEndAct
          filteredTransitions = transitionFiltering outgoingTransitionsOfCurrentAct ((intToFloat maxOccurrenceOfCurrentAct) * f)
      in filteredTransitions
        where
          getOutgoingTransitionsOfCurrentAct :: String -> [(Int, (String, String))] -> [(Int, (String, String))]
          getOutgoingTransitionsOfCurrentAct _ [] = []
          getOutgoingTransitionsOfCurrentAct currentAct ((occ, (act1, act2)):transitions) 
            | currentAct == act1 = (occ, (act1, act2)) : getOutgoingTransitionsOfCurrentAct currentAct transitions
            | otherwise = getOutgoingTransitionsOfCurrentAct currentAct transitions

          getOccurrenceOfCurrentAct :: String -> [(Int, [String])] -> Int
          getOccurrenceOfCurrentAct _ [] = 0
          getOccurrenceOfCurrentAct currentAct ((occ, endAct):endActOccurrences) 
            | [currentAct] == endAct = occ
            | otherwise = getOccurrenceOfCurrentAct currentAct endActOccurrences

          getMaxOccurrence :: Int -> [(Int, (String, String))] -> Int
          getMaxOccurrence res [] = res
          getMaxOccurrence res ((occ, (_, _)):xs) | occ > res = getMaxOccurrence occ xs
                                                  | otherwise = getMaxOccurrence res xs

          transitionFiltering :: [(Int, (String, String))] -> Float -> [(String, String)]
          transitionFiltering [] _ = []
          transitionFiltering ((occ,(x1,x2)):xs) relativeMaxOcc
            | (intToFloat occ) >= relativeMaxOcc = (x1,x2) : transitionFiltering xs relativeMaxOcc
            | otherwise = transitionFiltering xs relativeMaxOcc

sumValuesByTransitions :: [(Int, (String, String))] -> [(Int, (String, String))]
sumValuesByTransitions [] = []
sumValuesByTransitions (trace:traces) = sumValuesByTransitionsHelper trace traces [] []
  where
    sumValuesByTransitionsHelper :: (Int, (String, String)) -> [(Int, (String, String))] -> [(Int, (String, String))] -> [(Int, (String, String))] -> [(Int, (String, String))]
    sumValuesByTransitionsHelper (xOcc, xTrace) [] [] result = ((xOcc, xTrace):result)
    sumValuesByTransitionsHelper (xOcc, xTrace) [] (r:remaining) result = sumValuesByTransitionsHelper r remaining [] ((xOcc, xTrace):result)
    sumValuesByTransitionsHelper (xOcc, xTrace) ((yOcc, yTrace):ys) remaining result
      | xTrace == yTrace = sumValuesByTransitionsHelper (xOcc + yOcc, xTrace) ys remaining result
      | otherwise = sumValuesByTransitionsHelper (xOcc, xTrace) ys ((yOcc, yTrace):remaining) result

frequentStartAct :: Float -> [(Int, [String])] -> [String]
frequentStartAct f log =
  let startActOccurrences = removeTauListTraceInLog (sumValuesByTraces (map (\(occ, trace) -> (occ, [errorSafeHead trace])) log))
      maxOccurrence = maxOccurrenceAct 0 startActOccurrences
  in getFrequentAct startActOccurrences ((intToFloat maxOccurrence) * f)

frequentEndAct :: Float -> [(Int, [String])] -> [String]
frequentEndAct f log =
  let endActOccurrences = removeTauListTraceInLog (sumValuesByTraces (map (\(occ, trace) -> (occ, [last trace])) log))
      maxOccurrence = maxOccurrenceAct 0 endActOccurrences
  in getFrequentAct endActOccurrences ((intToFloat maxOccurrence) * f)

getFrequentAct :: [(Int, [String])] -> Float -> [String]
getFrequentAct [] _ = []
getFrequentAct ((occ, act):as) relativeMax
  | (intToFloat occ) < relativeMax = getFrequentAct as relativeMax
  | otherwise = concat act : getFrequentAct as relativeMax

maxOccurrenceAct :: Int -> [(Int, [String])] -> Int
maxOccurrenceAct res [] = res
maxOccurrenceAct res ((occ,[_]):xs) | occ > res = maxOccurrenceAct occ xs
                                    | otherwise = maxOccurrenceAct res xs
