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
simplifyUnit x [] = []
simplifyUnit x (y:ys)
    | x `elem` y = simplifyUnit x ys
    | -x `elem` y = (deleteBy (\z a -> a == -x) 0 y) : simplifyUnit x ys
    | otherwise = y : simplifyUnit x ys

unitPropagate :: [[Int]] -> [[Int]]
unitPropagate xs
    | y == Nothing = xs
    | otherwise = unitPropagate (simplifyUnit (fromJust y) xs)
        where y = findUnit xs

containsEmptyClause :: [[Int]] -> Bool
containsEmptyClause [] = False
containsEmptyClause (x:xs)
    | x == [] = True
    | otherwise = True && containsEmptyClause xs

pickLiteral :: [[Int]] -> Int
pickLiteral (x:xs) = head x

dpll :: [[Int]] -> Bool
dpll xs
    | containsEmptyClause (unitPropagate (simplifyUnit y xs)) && containsEmptyClause (unitPropagate (simplifyUnit (-y) xs)) = False 
    | otherwise = True
        where y = pickLiteral xs

main = do
    input <- getContents
    let clauseList = map getInts $ lines input
    if dpll clauseList == True then putStrLn "satisfiable" else putStrLn "unsatisfiable"
