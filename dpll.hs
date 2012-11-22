import Data.Char
import Data.List
import Data.Maybe
import System.IO

readCNFInput :: String -> [[Int]]
readCNFInput = map readCNFInput' . lines
    where readCNFInput' = map read . init . words

findUnit :: [[Int]] -> Maybe Int
findUnit [] = Nothing
findUnit (x:xs)
    | length x == 1 = Just $ head x
    | otherwise = findUnit xs

simplifyUnit :: Int -> [[Int]] -> [[Int]]
simplifyUnit x ys = map (filter (/= (-x))) (filter (x `notElem`) ys)

unitPropagate :: [[Int]] -> [[Int]]
unitPropagate xs
    | y == Nothing = xs
    | otherwise = unitPropagate (simplifyUnit (fromJust y) xs)
        where y = findUnit xs

containsEmptyClause :: [[Int]] -> Bool
containsEmptyClause xs
    | [] `elem` xs = True
    | otherwise = False

pickLiteral :: [[Int]] -> Int
pickLiteral (x:xs) = head x

dpll :: [[Int]] -> Bool
dpll xs
    | xs == [] = True
    | containsEmptyClause xs = False
    | otherwise = dpll (simplifyUnit z ys) || dpll (simplifyUnit (-z) ys)
        where ys = unitPropagate xs
              z = pickLiteral ys
        
main = do
    discard <- getLine
    input <- getContents
    let clauses = readCNFInput input
    if dpll clauses == True then putStrLn "satisfiable" else putStrLn "unsatisfiable"
