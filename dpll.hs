import Data.Maybe

-- Function: readCNFInput
--
-- Converts from DIMACS CNF input to a format we can deal with.
-- Literals are represented by ints, clauses are lists of literals, and the
-- formula is a list of clauses.
-- Note: This assumes the input follows the syntax:
-- p cnf 3 4 #--irrelevant as we are using lazy evaluation, so we drop this--#
-- -1 2 0
-- -1 -2 3 0
-- 1 0
-- -3 0 
readCNFInput :: String -> [[Int]]
readCNFInput = map readCNFInput' . lines
    where readCNFInput' = map read . init . words

-- Function: findUnit
-- 
-- Tries to find a unit clause in the given set of clauses. If it finds a unit
-- clause, it returns the single literal in the clause; otherwise, if there are
-- no unit clauses, it returns Nothing.
findUnit :: [[Int]] -> Maybe Int
findUnit [] = Nothing
findUnit [x]
    | length x == 1 = Just $ head x
    | otherwise = Nothing
findUnit (x:xs)
    | length x == 1 = Just $ head x
    | otherwise = findUnit xs

-- Function: simplifyUnit
-- 
-- Simplifies the given list of clauses assuming that the selected literal 
-- is true.
simplifyUnit :: Int -> [[Int]] -> [[Int]]
simplifyUnit x ys = map (filter (/= (-x))) $ filter (x `notElem`) ys

-- Function: unitPropagate
-- 
-- Takes a set of clauses and repeatedly applies unit propagation until it no
-- longer contains unit clauses.
unitPropagate :: [[Int]] -> [[Int]]
unitPropagate xs
    | y == Nothing = xs
    | otherwise = unitPropagate (simplifyUnit (fromJust y) xs)
        where y = findUnit xs

-- Function: containsEmptyClause
-- 
-- Returns True if the given set of clauses contains an empty clause.
containsEmptyClause :: [[Int]] -> Bool
containsEmptyClause xs
    | [] `elem` xs = True
    | otherwise = False

-- Function: pickLiteral
-- 
-- Selects the first literal from the list of clauses. 
-- Note: this assumes that the set of clauses is not empty, and that it does 
-- not contain empty clauses.
pickLiteral :: [[Int]] -> Int
pickLiteral (x:xs) = head x

-- Function: dpll
-- 
-- Returns True if the given set of clauses is satisfiable, and False otherwise.
dpll :: [[Int]] -> Bool
dpll xs
    | xs == [] = True
    | containsEmptyClause xs = False
    | otherwise = dpll (simplifyUnit z ys) || dpll (simplifyUnit (-z) ys)
        where ys = unitPropagate xs
              z = pickLiteral ys

-- Function: main
--
-- Defines the main logic of the program.
-- Reads a cnf formula from standard input, throws away the header and converts
-- it to our dpll input format. It writes "satisfiable"/"unsatisfiable" to 
-- the standard output depending on the result of the dpll function.
main = do
    discard <- getLine
    input <- getContents
    let clauses = readCNFInput input
    if dpll clauses then putStrLn "satisfiable" else putStrLn "unsatisfiable"
