import Data.Char
import Data.List
import Data.Maybe

getInts :: String -> [Int]
getInts xs
    | isValid xs = map read . init $ words xs
    | otherwise = []

isValid :: String -> Bool
isValid [] = False
isValid [x] = not $ isAlpha x
isValid (x:xs) = isValid [x] && isValid xs

findUnit :: [[Int]] -> Maybe Int
findUnit (x:xs)
    | length x == 1 = Just $ head x
    | otherwise = Nothing

simplifyUnit :: Int -> [[Int]] -> [[Int]]
simplifyUnit x (y:ys)
    | x `elem` y = simplifyUnit x ys -- TODO: This should remove the element only, not the entire list!
    | -x `elem` y = [] : simplifyUnit x ys -- TODO: Find out what this is actually supposed to do.
    | otherwise = y : simplifyUnit x ys -- TODO: Find out what this is actually supposed to do.

unitPropagate :: [[Int]] -> [[Int]]
unitPropagate xs
    | y == Nothing = xs
    | otherwise = unitPropagate (simplifyUnit (fromJust y) xs)
        where y = findUnit xs

containsEmptyClause :: [[Int]] -> Bool
containsEmptyClause (x:xs)
    | x == [] = True
    | otherwise = True && containsEmptyClause xs

pickLiteral :: [[Int]] -> Int
pickLiteral (x:xs) = head x

dpll :: [[Int]] -> Bool
dpll xs
    | containsEmptyClause (unitPropagate (simplifyUnit y xs)) = False 
    | containsEmptyClause (unitPropagate (simplifyUnit (-y) xs)) = False
    | otherwise = True
        where y = pickLiteral xs

main = do
    input <- getContents
    let clauseList = map getInts $ lines input
    if dpll clauseList == True then putStrLn "satisfiable" else putStrLn "unsatisfiable"
