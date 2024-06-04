{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import Control.Monad
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Array
import Data.List (sort, sortBy, subsequences, sortOn, findIndices, findIndex)
import Data.Ord (comparing, Down)
import Debug.Trace (traceShow)
import System.Environment (getArgs)
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude 
import System.Random (randomRIO)
import Control.Monad.State
import Surely (solve)
import Data.Text (unpack, pack)
import System.IO
import Control.DeepSeq (deepseq)

data Color1 = Black | Red | Blue | Green | Yellow | Purple | Orange | LightBlue | Cyan | Magenta | Pink | Turquoise | Lavender | Coral | White
  deriving (Eq, Show)
-- Los numeros 20 al 34 se reservan para los interiores de los caminos del color n-20
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
cell :: Int -> Int -> Double -> Diagram B
cell num old size =
  let
    squareDiagram = square squareSize # lw (local 0.02) # lc black # fc (colorToColor (intToColor num))
    textDiagram = if num /= 0 && old /= 0 then text (show num) # fontSizeL fontSize # centerX # centerY else mempty
  in
    textDiagram <> squareDiagram
  where
    squareSize = size
    fontSize = squareSize / 2

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
row :: [Int] -> [Int] -> Double -> Diagram B
row nums old size = hcat (zipWith (\n1 n2 -> cell n1 n2 size) nums old)

-- Función para obtener el tamaño del tablero
boardSize :: Int
--boardSize = length exampleBoard
boardSize = 10

-- Función para dibujar un tablero completo
board :: [[Int]] -> [[Int]] -> Diagram B
--board nums = vcat (map (\r -> row r size) nums)
board nums old = vcat (zipWith (\r1 r2 -> row r1 r2 (fromIntegral size)) nums old)
  where
    size = length nums -- tamaño base del tablero

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


parseTablero :: String -> Maybe (Array (Int, Int) Int)
parseTablero str = do
  let parse = map (map read . words) (filter (not . null) $ lines str) :: [[Int]]
  let (dimensiones : tablero) = parse
  if length dimensiones /= 2
    then Nothing
    else do
      let filas = head dimensiones
      let columnas = dimensiones !! 1
      let tablero_aux = concat tablero
      if null tablero || length tablero_aux /= filas * columnas
        then Nothing
        else do
          return $ listArray ((1, 1), (filas, columnas)) tablero_aux

esTableroValido :: Array (Int, Int) Int -> Maybe [(Int, (Int, Int), (Int, Int))]
esTableroValido tablero = do
  let posiciones = sortBy (comparing snd) (filter (\(pos, num) -> num /= 0) (assocs tablero))
  let (filas, columnas) = snd (bounds tablero)
  if (length posiciones <= filas + columnas) && dosVeces (map snd posiciones)
    then Just $ Main.union posiciones
    else Nothing

dosVeces :: [Int] -> Bool
dosVeces [] = True
dosVeces [_] = False
dosVeces [x, y] = x == y
dosVeces (x : y : z : l) = x == y && x /= z && dosVeces (z : l)

union :: [((Int, Int), Int)] -> [(Int, (Int, Int), (Int, Int))]
union [] = []
union ((pos1, num) : (pos2, _) : x) = (num, pos1, pos2) : Main.union x

listaArray :: Array (Int, Int) Int -> [[Int]]
listaArray tablero = do
  let (filas, columnas) = snd (bounds tablero)
  [[tablero! (x, y) | y <- [1.. columnas]] | x <- [1.. filas]]

vecinos :: (Int, Int) -> Array (Int, Int) Int -> [(Int, Int)]
vecinos (x, y) tablero = filter (inRange (bounds tablero)) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

clausulaCelda :: (Int, Int) -> Array (Int, Int) Int -> [[Int]]
clausulaCelda (x, y) tablero = if esCeldaFinal
                                  then [[celdaFinal]] ++ notOtherColorsClauses ++ [neighborColors] ++ neighborClauses
                                  else [colorClauses] ++ notTwoColorsClauses ++ [pipeClauses] ++ notTwoPipesClauses ++ matchingPipes ++ notMatchingPipes
  where
    ((_, _), (numFilas, _)) = bounds tablero
    colorClauses = [(x * numFilas + y) * numFilas + i - ((numFilas * numFilas) + numFilas) | i <- [1..numFilas]]
    notTwoColorsClauses = [ [-u, -v] | [u, v] <- combinations 2 colorClauses ]
    esCeldaFinal = tablero ! (x, y) /= 0
    celdaFinal = (x * numFilas + y) * numFilas + tablero ! (x, y) - ((numFilas * numFilas) + numFilas)
    notOtherColorsClauses = map (\i -> [-i]) $ filter (/= celdaFinal) colorClauses
    neighborColors = [ color | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)], 
                   let nx = x + dx, 
                   let ny = y + dy, 
                   nx >= 1, nx <= numFilas, 
                   ny >= 1, ny <= numFilas, 
                   let color = (nx * numFilas + ny) * numFilas + (tablero ! (x, y)) - ((numFilas * numFilas) + numFilas) ]
    neighborClauses = [ [-u, -v] | [u, v] <- filter ((2==) . length) (combinations 2 neighborColors) ]
    pipeClauses = if (x == 1 && y == 1) then [(x * numFilas + y) * numFilas + 6 - ((numFilas * numFilas) + numFilas) + (numFilas * numFilas * numFilas)]
                  else if (x == 1 && y == numFilas) then [(x * numFilas + y) * numFilas + 5 - ((numFilas * numFilas) + numFilas) + (numFilas * numFilas * numFilas)]
                  else if (x == numFilas && y == 1) then [(x * numFilas + y) * numFilas + 5 - ((numFilas * numFilas) + numFilas) + (numFilas * numFilas * numFilas)]
                  else if (x == numFilas && y == numFilas) then [(x * numFilas + y) * numFilas + 3 - ((numFilas * numFilas) + numFilas) + (numFilas * numFilas * numFilas)]
                  else if (x == 1) then [(x * numFilas + y) * numFilas + i - ((numFilas * numFilas) + numFilas) + (numFilas * numFilas * numFilas) | i <- [1,5,6]]
                  else if (x == numFilas) then [(x * numFilas + y) * numFilas + i - ((numFilas * numFilas) + numFilas) + (numFilas * numFilas * numFilas) | i <- [1,3,4]]
                  else if (y == 1) then [(x * numFilas + y) * numFilas + i - ((numFilas * numFilas) + numFilas) + (numFilas * numFilas * numFilas) | i <- [2,4,6]]
                  else if (y == numFilas) then [(x * numFilas + y) * numFilas + i - ((numFilas * numFilas) + numFilas) + (numFilas * numFilas * numFilas) | i <- [2,3,5]]
                  else [(x * numFilas + y) * numFilas + i - ((numFilas * numFilas) + numFilas) + (numFilas * numFilas * numFilas) | i <- [1..6]]
    pipeClause = (x * numFilas + y) * numFilas - ((numFilas * numFilas) + numFilas) + (numFilas * numFilas * numFilas)
    notTwoPipesClauses =  [ [-u, -v] | [u, v] <- combinations 2 pipeClauses ]
    matchingPipes = if (x == 1 && y == 1) then [[-(pipeClause + 6), -(colorClauses !! i), ((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 6), (colorClauses !! i), -((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 6), -(colorClauses !! i), (((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 6), (colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
                  else if (x == 1 && y == numFilas) then [[-(pipeClause + 5), -(colorClauses !! i), ((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 5), (colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                               [[-(pipeClause + 5), -(colorClauses !! i), (((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 5), (colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
                  else if (x == numFilas && y == 1) then [[-(pipeClause + 4), -(colorClauses !! i), ((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 4), (colorClauses !! i), -((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 4), -(colorClauses !! i), (((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 4), (colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
                  else if (x == numFilas && y == numFilas) then [[-(pipeClause + 3), -(colorClauses !! i), ((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 3), (colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 3), -(colorClauses !! i), (((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 3), (colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
                  else if (x == 1) then [[-(pipeClause + 6), -(colorClauses !! i), ((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 6), (colorClauses !! i), -((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 6), -(colorClauses !! i), (((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 6), (colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 5), -(colorClauses !! i), ((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 5), (colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                               [[-(pipeClause + 5), -(colorClauses !! i), (((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 5), (colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 1), -(colorClauses !! i), ((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 1), (colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ 
                                                [[-(pipeClause + 1), -(colorClauses !! i), ((x * numFilas + (y + 1)) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 1), (colorClauses !! i), -((x * numFilas + (y + 1)) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
                  else if (x == numFilas) then [[-(pipeClause + 4), -(colorClauses !! i), ((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 4), (colorClauses !! i), -((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 4), -(colorClauses !! i), (((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 4), (colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 3), -(colorClauses !! i), ((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 3), (colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 3), -(colorClauses !! i), (((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 3), (colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 1), -(colorClauses !! i), ((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 1), (colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ 
                                                [[-(pipeClause + 1), -(colorClauses !! i), ((x * numFilas + (y + 1)) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 1), (colorClauses !! i), -((x * numFilas + (y + 1)) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
                  else if (y == 1) then [[-(pipeClause + 6), -(colorClauses !! i), ((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 6), (colorClauses !! i), -((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 6), -(colorClauses !! i), (((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 6), (colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 4), -(colorClauses !! i), ((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 4), (colorClauses !! i), -((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 4), -(colorClauses !! i), (((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 4), (colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 2), -(colorClauses !! i), (((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 2), (colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 2), -(colorClauses !! i), (((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 2), (colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
                  else if (y == numFilas) then [[-(pipeClause + 3), -(colorClauses !! i), ((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 3), (colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 3), -(colorClauses !! i), (((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 3), (colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 5), -(colorClauses !! i), ((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 5), (colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                               [[-(pipeClause + 5), -(colorClauses !! i), (((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 5), (colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                 [[-(pipeClause + 2), -(colorClauses !! i), (((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 2), (colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 2), -(colorClauses !! i), (((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 2), (colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
                  else [[-(pipeClause + 1), -(colorClauses !! i), ((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 1), (colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ 
                                                [[-(pipeClause + 1), -(colorClauses !! i), ((x * numFilas + (y + 1)) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 1), (colorClauses !! i), -((x * numFilas + (y + 1)) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                        [[-(pipeClause + 2), -(colorClauses !! i), (((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 2), (colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 2), -(colorClauses !! i), (((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 2), (colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                        [[-(pipeClause + 3), -(colorClauses !! i), ((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 3), (colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 3), -(colorClauses !! i), (((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 3), (colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                        [[-(pipeClause + 4), -(colorClauses !! i), ((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 4), (colorClauses !! i), -((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 4), -(colorClauses !! i), (((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 4), (colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                        [[-(pipeClause + 5), -(colorClauses !! i), ((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 5), (colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                               [[-(pipeClause + 5), -(colorClauses !! i), (((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 5), (colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                        [[-(pipeClause + 6), -(colorClauses !! i), ((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 6), (colorClauses !! i), -((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                                                [[-(pipeClause + 6), -(colorClauses !! i), (((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 6), (colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
    notMatchingPipes = if (x == 1 && y /= 1 && y /= numFilas) then [[-(pipeClause + 6), -(colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 5), -(colorClauses !! i), -((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 1), -(colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
                        else if (x == numFilas && y /= 1 && y /= numFilas) then [[-(pipeClause + 4), -(colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 3), -(colorClauses !! i), -((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 1), -(colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
                        else if (y == 1 && x /= 1 && x /= numFilas) then [[-(pipeClause + 6), -(colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 4), -(colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 2), -(colorClauses !! i), -((x * numFilas + (y + 1)) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
                        else if (y == numFilas && x /= 1 && x /= numFilas) then [[-(pipeClause + 5), -(colorClauses !! i), -(((x - 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 3), -(colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 2), -(colorClauses !! i), -((x * numFilas + (y - 1)) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
                        else if (x /= 1 && x /= numFilas && y /= 1 && y /= numFilas) then [[-(pipeClause + 6), -(colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 5), -(colorClauses !! i), -((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 1), -(colorClauses !! i), -(((x + 1) * numFilas + y) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++
                              [[-(pipeClause + 4), -(colorClauses !! i), -((x * numFilas + y - 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++ [[-(pipeClause + 3), -(colorClauses !! i), -((x * numFilas + y + 1) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]] ++  [[-(pipeClause + 2), -(colorClauses !! i), -((x * numFilas + (y + 1)) * numFilas + (i+1) - ((numFilas * numFilas) + numFilas))] | i <- [0..numFilas-1]]
                        else []

combinations :: Int -> [a] -> [[a]]
combinations n xs = filter ((n==) . length) (subsequences xs)

clausulasColor :: Array (Int, Int) Int -> [[Int]]
clausulasColor tablero = concatMap (\(x, _) -> clausulaCelda x tablero) (assocs tablero)

convertirAInteger :: [[Int]] -> [[Integer]]
convertirAInteger = map (map fromIntegral)

ordenarPorValorAbsoluto :: [Int] -> [Int]
ordenarPorValorAbsoluto = sortOn abs

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = take n xs : splitEvery n (drop n xs)

colorMatrix :: [[Int]] -> [Int]
colorMatrix xs = map colorCell xs
  where
    colorCell sublist = case findIndex (>0) sublist of
                          Just idx -> idx + 1
                          Nothing  -> error "No positive number found in sublist"

convertirAIO :: [[Int]] -> IO [[Int]]
convertirAIO x = return x

resolver :: [Char] -> IO()
resolver tab = do
  let tablero = parseTablero tab
  case tablero of
    Nothing -> putStrLn "Error fichero"
    Just tablero -> do
      let tableroValido = esTableroValido tablero
      case tableroValido of
        Nothing -> putStrLn "Tablero inválido"
        Just tableroValido -> do
          let clausulas = clausulasColor tablero
          --print (clausulas)
          let ((_, _), (numFilas, _)) = bounds (tablero)
          let solucion = case solve (convertirAInteger clausulas) of
                  Just val -> ordenarPorValorAbsoluto (map fromIntegral val)
                  Nothing  -> []
          --print(solucion)
          let solucionAgrupada = take (numFilas * numFilas) (splitEvery numFilas solucion)
          --print (solucionAgrupada)
          --let count = 0
          let solution = splitEvery numFilas (colorMatrix (solucionAgrupada))
          solutionIO <- convertirAIO solution
          --solution <- splitEvery numFilas (colorMatrix (solucionAgrupada))
          --print (solution)
          mainWith (board solutionIO (listaArray tablero))

main :: IO ()
main = do
          Gtk.init Nothing

          builder <- Gtk.builderNewFromFile "src-exe/Flow1.glade"
          buscarButtonObj <- Gtk.builderGetObject builder "buscar"
          buscarButton <- case buscarButtonObj of
              Nothing -> error "No se pudo encontrar el objeto 'buscar'"
              Just obj -> Gtk.unsafeCastTo Gtk.Button obj
          nombreArchivoEntryObj <- Gtk.builderGetObject builder "nombreArchivo"
          nombreArchivoEntry <- case nombreArchivoEntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'nombreArchivo'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          resultadoImageObj <- Gtk.builderGetObject builder "resultado"
          resultadoImage <- case resultadoImageObj of
              Nothing -> error "No se pudo encontrar el objeto 'resultado'"
              Just obj -> Gtk.unsafeCastTo Gtk.Image obj
          windowObj <- Gtk.builderGetObject builder "FlowFree"
          window <- case windowObj of
            Nothing -> error "No se pudo encontrar el objeto 'App'"
            Just obj -> Gtk.unsafeCastTo Gtk.Window obj

          Gtk.on buscarButton #clicked $ do
              texto <- Gtk.entryGetText nombreArchivoEntry
              -- Ahora 'texto' contiene el texto del campo de entrada. Puedes usarlo como quieras.
              contenido <- readFile $ unpack texto
              resolver contenido
              Gtk.imageSetFromFile resultadoImage (Just "./src-exe/output.svg")


          Gtk.on window #destroy Gtk.mainQuit

          #showAll window

          Gtk.main