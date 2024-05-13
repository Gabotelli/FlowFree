import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

type Point = (Int, Int)
type Pair = (Point, Point)
type Matrix = [[Int]]

-- Read data from file
readData :: FilePath -> IO [[Point]]
readData file = do
    content <- readFile file
    let [nSize, mSize, pairs] = map read (lines content)
    return pairs

-- Solve the problem
solve :: Int -> Int -> [Pair] -> Maybe [[Point]]
solve nSize mSize pairs = do
	let matrix = emptyMatrix nSize mSize
	let startMatrix' = startMatrix pairs matrix 1
	findSolution pairs startMatrix' 1 []

-- Create an empty matrix
emptyMatrix :: Int -> Int -> Matrix
emptyMatrix nSize mSize = replicate nSize (replicate mSize 0)

-- Set start positions on matrix
startMatrix :: [Pair] -> Matrix -> Int -> Matrix
startMatrix [] matrix _ = matrix
startMatrix (((n1, m1),(n2, m2)) : ps) matrix acc =
  let matrix' = updateMatrix matrix (n1, m1) acc
      matrix'' = updateMatrix matrix' (n2, m2) acc
   in startMatrix ps matrix'' (acc + 1)

-- Find solution
findSolution :: [Pair] -> Matrix -> Int -> [[Point]] -> Maybe [[Point]]
findSolution [] _ _ result = Just result
findSolution (((sn, sm), (dn, dm)) : ps) matrix num result =
	case findPath sn sm dn dm matrix [] of
		Just route -> 
			let matrix' = fillWithNum matrix route num
			in findSolution ps matrix' (num + 1) (route : result)
		Nothing -> Nothing

-- Find a path between two points
findPath :: Int -> Int -> Int -> Int -> Matrix -> [(Int, Int)] -> Maybe [(Int, Int)]
findPath n1 m1 n2 m2 matrix visited
	| (n1, m1) == (n2, m2) = Just ((n1, m1) : visited)
	| otherwise =
		let connectedPoints = filter (\(x, y) -> x >= 1 && y >= 1 && x <= length matrix && y <= length (head matrix) && node matrix x y == 0) $ [(n1, m1 - 1), (n1, m1 + 1), (n1 - 1, m1), (n1 + 1, m1)]
		in let nextPoints = filter (\p -> p `notElem` visited) connectedPoints
		in case nextPoints of
			[] -> Nothing
			_ -> head <$> mapM (\(n, m) -> findPath n m n2 m2 matrix ((n, m) : visited)) nextPoints

-- Update matrix with a number at a point
updateMatrix :: Matrix -> Point -> Int -> Matrix
updateMatrix matrix (n, m) num =
    let (before, row : after) = splitAt (n - 1) matrix
        (left, _ : right) = splitAt (m - 1) row
    in before ++ [left ++ [num] ++ right] ++ after

-- Get the value at a point in the matrix
node :: Matrix -> Int -> Int -> Int
node matrix n m = matrix !! (n - 1) !! (m - 1)

-- Fill matrix with a number along a path
fillWithNum :: Matrix -> [Point] -> Int -> Matrix
fillWithNum matrix path num = foldl (\m p -> updateMatrix m p num) matrix path

main :: IO ()
main = do
	let testPairs = [((1, 1), (1, 6)), ((1, 2), (3, 5)), ((2, 2), (3, 4))]
	let testResult =
				[ [(1, 1), (2, 1), (3, 1), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5), (4, 6), (3, 6), (2, 6), (1, 6)],
					[(1, 2), (1, 3), (2, 3), (2, 4), (2, 5), (3, 5)],
					[(2, 2), (3, 2), (3, 3), (3, 4)]
				]

	putStrLn "Testing with provided pairs:"
	putStrLn "Expected Result:"
	print testResult
	putStrLn "Actual Result:"
	let result = solve 4 6 testPairs
	case result of
		Just res -> print res
		Nothing -> putStrLn "No solution found."
