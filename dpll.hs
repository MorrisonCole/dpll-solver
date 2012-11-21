import Data.List

solveDPLL :: [[Int]] -> Bool
solveDPLL = foldr (||) False . map solveClause

solveClause :: [Int] -> Bool
solveClause [] = False
solveClause [x] = True
solveClause xs = xs == nubBy (\x y -> x == -y) xs

main = do
    let testList = [[4,5,6,-4],[5, -5],[2,-2]]
    if solveDPLL testList == True then putStrLn "satisfiable" else putStrLn "unsatisfiable"
