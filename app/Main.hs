import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import System.Random (randomRIO)
import Control.Monad.State

data Color1 = Black | Red | Blue | Green | Yellow | Purple | Orange | LightBlue | Cyan | Magenta | Pink | Turquoise | Lavender | Coral | White
  deriving (Eq, Show)

-- Función para convertir un número a un Color1
intToColor :: Int -> Color1
intToColor 0 = Black
intToColor 1 = Red
intToColor 2 = Blue
intToColor 3 = Green
intToColor 4 = Yellow
intToColor 5 = Purple
intToColor 6 = Orange
intToColor 7 = LightBlue
intToColor 8 = Cyan
intToColor 9 = Magenta
intToColor 10 = Pink
intToColor 11 = Turquoise
intToColor 12 = Lavender
intToColor 13 = Coral
intToColor 14 = White
intToColor _ = Black -- Consideramos cualquier otro número como negro

-- Función para dibujar una casilla
cell :: Int -> Double -> Diagram B
cell num size =
  (if num /= 0 then text (show num) # fontSizeL fontSize else mempty)
    <> square squareSize # lw none # fc (colorToColor (intToColor num))
  where
    squareSize = size / fromIntegral boardSize
    fontSize = min (squareSize / 2) 0.5

-- Función para convertir un Color1 a un Color
colorToColor :: Color1 -> Colour Double
colorToColor Black = black
colorToColor Red = red
colorToColor Blue = blue
colorToColor Green = green
colorToColor Yellow = yellow
colorToColor Purple = purple
colorToColor Orange = orange
colorToColor LightBlue = lightblue
colorToColor Cyan = cyan
colorToColor Magenta = magenta
colorToColor Pink = pink
colorToColor Turquoise = turquoise
colorToColor Lavender = lavender
colorToColor Coral = coral
colorToColor White = white

-- Función para dibujar una fila de casillas
row :: [Int] -> Double -> Diagram B
row nums size = hcat (map (\n -> cell n size) nums)

-- Función para obtener el tamaño del tablero
boardSize :: Int
boardSize = length exampleBoard

-- Función para dibujar un tablero completo
board :: [[Int]] -> Diagram B
board nums = vcat (map (\r -> row r size) nums)
  where
    size = 10 -- tamaño base del tablero

-- Ejemplo de un tablero 8x8 con números asignados
exampleBoard2 :: [[Int]]
exampleBoard2 =
  [ [3, 1, 0, 0, 7, 0, 0, 7],
    [0, 0, 3, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 5, 0, 0, 0],
    [0, 0, 2, 0, 0, 0, 0, 0],
    [0, 0, 4, 0, 0, 0, 0, 0],
    [0, 0, 0, 2, 4, 6, 0, 0],
    [6, 0, 0, 0, 0, 0, 0, 0],
    [5, 0, 1, 0, 0, 0, 0, 0]
  ]

exampleBoard :: [[Int]]
exampleBoard =
  [ [12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 4, 0, 0],
    [0, 0, 0, 0, 0, 14, 0, 0, 1, 0, 0, 1, 5, 0],
    [0, 6, 0, 0, 0, 9, 12, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 4, 7, 0, 0, 8, 0, 0, 3, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 14, 0, 11, 0, 10, 13, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 5, 0, 0],
    [0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  ]

exampleBoard3 :: [[Int]]
exampleBoard3 =
  [ [1, 0, 3, 0, 4],
    [0, 0, 2, 0, 5],
    [0, 0, 0, 0, 0],
    [0, 3, 0, 4, 0],
    [0, 1, 2, 5, 0]
  ]

exampleBoard4 :: [[Int]]
exampleBoard4 =
  [ [0, 0, 1, 4],
    [0, 2, 3, 0],
    [1, 0, 0, 0],
    [2, 0, 3, 4]
  ]

exampleBoard5 :: [[Int]]
exampleBoard5 =
  [ [0, 1, 3],
    [0, 2, 0],
    [1, 2, 3]
  ]

fillRandom :: [[Int]] -> IO [[Int]]
fillRandom = mapM (mapM fillCell)
  where
    fillCell 0 = randomRIO (1, 4) -- Rellena una celda vacía con un número aleatorio entre 1 y 14
    fillCell x = return x -- Deja las celdas no vacías como están

-- Función para obtener los vecinos de una celda
getNeighbors :: [[Int]] -> (Int, Int) -> [Int]
getNeighbors matrix (i, j) = [matrix !! x !! y | (x, y) <- [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)], x >= 0, y >= 0, x < length matrix, y < length (matrix !! 0)]

checkMatrix2 :: [[Int]] -> [[Int]] -> Bool
checkMatrix2 matrix1 matrix2 =
  all
    ( \(i, row) ->
        all
          ( \(j, cell1) ->
              let cell2 = (matrix2 !! i) !! j
               in (cell1 == 0 && cell2 /= 0 && length (filter (== cell2) (getNeighbors matrix2 (i, j))) == 2) || (cell1 /= 0 && length (filter (== cell2) (getNeighbors matrix2 (i, j))) == 1)
          )
          (zip [0 ..] row)
    )
    (zip [0 ..] matrix1)

generateValidBoard :: [[Int]] -> Int -> IO [[Int]]
generateValidBoard matrix count = do
  putStrLn (show count) -- Imprime un mensaje
  newMatrix <- fillRandom matrix
  if checkMatrix2 matrix newMatrix
    then return newMatrix
    else generateValidBoard matrix (count + 1)

main :: IO ()
main = do
  let count = 0
  solution <- generateValidBoard exampleBoard4 count
  mainWith (board solution)
