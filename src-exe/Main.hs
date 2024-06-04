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
import Data.Text (unpack, pack, Text)
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

comparar :: [[Text]] -> [[Int]] -> IO()
comparar m n = do
    let m' = map (map (read . unpack)) m
    if m' == n
        then putStrLn "Las matrices son iguales."
        else putStrLn "Las matrices no son iguales."

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
          c115x5EntryObj <- Gtk.builderGetObject builder "c115x5"
          c115x5Entry <- case c115x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c115x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c125x5EntryObj <- Gtk.builderGetObject builder "c125x5"
          c125x5Entry <- case c125x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c125x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c135x5EntryObj <- Gtk.builderGetObject builder "c135x5"
          c135x5Entry <- case c135x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c135x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c145x5EntryObj <- Gtk.builderGetObject builder "c145x5"
          c145x5Entry <- case c145x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c145x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c155x5EntryObj <- Gtk.builderGetObject builder "c155x5"
          c155x5Entry <- case c155x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c155x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c215x5EntryObj <- Gtk.builderGetObject builder "c215x5"
          c215x5Entry <- case c215x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c215x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c225x5EntryObj <- Gtk.builderGetObject builder "c225x5"
          c225x5Entry <- case c225x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c225x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c235x5EntryObj <- Gtk.builderGetObject builder "c235x5"
          c235x5Entry <- case c235x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c235x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c245x5EntryObj <- Gtk.builderGetObject builder "c245x5"
          c245x5Entry <- case c245x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c245x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c255x5EntryObj <- Gtk.builderGetObject builder "c255x5"
          c255x5Entry <- case c255x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c255x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c315x5EntryObj <- Gtk.builderGetObject builder "c315x5"
          c315x5Entry <- case c315x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c315x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c325x5EntryObj <- Gtk.builderGetObject builder "c325x5"
          c325x5Entry <- case c325x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c325x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c335x5EntryObj <- Gtk.builderGetObject builder "c335x5"
          c335x5Entry <- case c335x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c335x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c345x5EntryObj <- Gtk.builderGetObject builder "c345x5"
          c345x5Entry <- case c345x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c345x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c355x5EntryObj <- Gtk.builderGetObject builder "c355x5"
          c355x5Entry <- case c355x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c355x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c415x5EntryObj <- Gtk.builderGetObject builder "c415x5"
          c415x5Entry <- case c415x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c415x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c425x5EntryObj <- Gtk.builderGetObject builder "c425x5"
          c425x5Entry <- case c425x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c425x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c435x5EntryObj <- Gtk.builderGetObject builder "c435x5"
          c435x5Entry <- case c435x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c435x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c445x5EntryObj <- Gtk.builderGetObject builder "c445x5"
          c445x5Entry <- case c445x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c445x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c455x5EntryObj <- Gtk.builderGetObject builder "c455x5"
          c455x5Entry <- case c455x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c455x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c515x5EntryObj <- Gtk.builderGetObject builder "c515x5"
          c515x5Entry <- case c515x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c515x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c525x5EntryObj <- Gtk.builderGetObject builder "c525x5"
          c525x5Entry <- case c525x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c525x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c535x5EntryObj <- Gtk.builderGetObject builder "c535x5"
          c535x5Entry <- case c535x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c535x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c545x5EntryObj <- Gtk.builderGetObject builder "c545x5"
          c545x5Entry <- case c545x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c545x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c555x5EntryObj <- Gtk.builderGetObject builder "c555x5"
          c555x5Entry <- case c555x5EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c555x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj

          
          resolver5x5ButtonObj <- Gtk.builderGetObject builder "resolver5x5"
          resolver5x5Button <- case resolver5x5ButtonObj of
              Nothing -> error "No se pudo encontrar el objeto 'resolver5x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Button obj

          Gtk.on buscarButton #clicked $ do
              texto <- Gtk.entryGetText nombreArchivoEntry
              -- Ahora 'texto' contiene el texto del campo de entrada. Puedes usarlo como quieras.
              contenido <- readFile $ unpack texto
              resolver contenido
              Gtk.imageSetFromFile resultadoImage (Just "./src-exe/output.svg")

          Gtk.on resolver5x5Button #clicked $ do
              c115x5 <- Gtk.entryGetText c115x5Entry
              c125x5 <- Gtk.entryGetText c125x5Entry
              c135x5 <- Gtk.entryGetText c135x5Entry
              c145x5 <- Gtk.entryGetText c145x5Entry
              c155x5 <- Gtk.entryGetText c155x5Entry
              c215x5 <- Gtk.entryGetText c215x5Entry
              c225x5 <- Gtk.entryGetText c225x5Entry
              c235x5 <- Gtk.entryGetText c235x5Entry
              c245x5 <- Gtk.entryGetText c245x5Entry
              c255x5 <- Gtk.entryGetText c255x5Entry
              c315x5 <- Gtk.entryGetText c315x5Entry
              c325x5 <- Gtk.entryGetText c325x5Entry
              c335x5 <- Gtk.entryGetText c335x5Entry
              c345x5 <- Gtk.entryGetText c345x5Entry
              c355x5 <- Gtk.entryGetText c355x5Entry
              c415x5 <- Gtk.entryGetText c415x5Entry
              c425x5 <- Gtk.entryGetText c425x5Entry
              c435x5 <- Gtk.entryGetText c435x5Entry
              c445x5 <- Gtk.entryGetText c445x5Entry
              c455x5 <- Gtk.entryGetText c455x5Entry
              c515x5 <- Gtk.entryGetText c515x5Entry
              c525x5 <- Gtk.entryGetText c525x5Entry
              c535x5 <- Gtk.entryGetText c535x5Entry
              c545x5 <- Gtk.entryGetText c545x5Entry
              c555x5 <- Gtk.entryGetText c555x5Entry

              let m = [[c115x5, c125x5, c135x5, c145x5, c155x5], 
                      [c215x5, c225x5, c235x5, c245x5, c255x5], 
                      [c315x5, c325x5, c335x5, c345x5, c355x5], 
                      [c415x5, c425x5, c435x5, c445x5, c455x5], 
                      [c515x5, c525x5, c535x5, c545x5, c555x5]]

              comparar m [[1,4,4,2,3],[1,4,5,2,3],[1,4,5,2,3],[1,1,5,2,3],[3,3,3,3,3]]
              
          Gtk.on window #destroy Gtk.mainQuit

          #showAll window

          Gtk.main