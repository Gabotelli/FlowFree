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
          c116x6EntryObj <- Gtk.builderGetObject builder "c116x6"
          c116x6Entry <- case c116x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c116x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c126x6EntryObj <- Gtk.builderGetObject builder "c126x6"
          c126x6Entry <- case c126x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c126x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c136x6EntryObj <- Gtk.builderGetObject builder "c136x6"
          c136x6Entry <- case c136x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c136x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c146x6EntryObj <- Gtk.builderGetObject builder "c146x6"
          c146x6Entry <- case c146x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c146x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c156x6EntryObj <- Gtk.builderGetObject builder "c156x6"
          c156x6Entry <- case c156x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c156x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c166x6EntryObj <- Gtk.builderGetObject builder "c166x6"
          c166x6Entry <- case c166x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c166x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c216x6EntryObj <- Gtk.builderGetObject builder "c216x6"
          c216x6Entry <- case c216x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c216x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c226x6EntryObj <- Gtk.builderGetObject builder "c226x6"
          c226x6Entry <- case c226x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c226x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c236x6EntryObj <- Gtk.builderGetObject builder "c236x6"
          c236x6Entry <- case c236x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c236x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c246x6EntryObj <- Gtk.builderGetObject builder "c246x6"
          c246x6Entry <- case c246x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c246x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c256x6EntryObj <- Gtk.builderGetObject builder "c256x6"
          c256x6Entry <- case c256x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c256x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c266x6EntryObj <- Gtk.builderGetObject builder "c266x6"
          c266x6Entry <- case c266x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c266x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c316x6EntryObj <- Gtk.builderGetObject builder "c316x6"
          c316x6Entry <- case c316x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c316x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c326x6EntryObj <- Gtk.builderGetObject builder "c326x6"
          c326x6Entry <- case c326x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c326x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c336x6EntryObj <- Gtk.builderGetObject builder "c336x6"
          c336x6Entry <- case c336x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c336x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c346x6EntryObj <- Gtk.builderGetObject builder "c346x6"
          c346x6Entry <- case c346x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c346x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c356x6EntryObj <- Gtk.builderGetObject builder "c356x6"
          c356x6Entry <- case c356x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c356x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c366x6EntryObj <- Gtk.builderGetObject builder "c366x6"
          c366x6Entry <- case c366x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c366x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c416x6EntryObj <- Gtk.builderGetObject builder "c416x6"
          c416x6Entry <- case c416x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c416x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c426x6EntryObj <- Gtk.builderGetObject builder "c426x6"
          c426x6Entry <- case c426x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c426x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c436x6EntryObj <- Gtk.builderGetObject builder "c436x6"
          c436x6Entry <- case c436x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c436x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c446x6EntryObj <- Gtk.builderGetObject builder "c446x6"
          c446x6Entry <- case c446x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c446x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c456x6EntryObj <- Gtk.builderGetObject builder "c456x6"
          c456x6Entry <- case c456x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c456x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c466x6EntryObj <- Gtk.builderGetObject builder "c466x6"
          c466x6Entry <- case c466x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c466x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c516x6EntryObj <- Gtk.builderGetObject builder "c516x6"
          c516x6Entry <- case c516x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c516x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c526x6EntryObj <- Gtk.builderGetObject builder "c526x6"
          c526x6Entry <- case c526x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c526x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c536x6EntryObj <- Gtk.builderGetObject builder "c536x6"
          c536x6Entry <- case c536x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c536x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c546x6EntryObj <- Gtk.builderGetObject builder "c546x6"
          c546x6Entry <- case c546x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c546x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c556x6EntryObj <- Gtk.builderGetObject builder "c556x6"
          c556x6Entry <- case c556x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c556x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c566x6EntryObj <- Gtk.builderGetObject builder "c566x6"
          c566x6Entry <- case c566x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c566x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c616x6EntryObj <- Gtk.builderGetObject builder "c616x6"
          c616x6Entry <- case c616x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c616x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c626x6EntryObj <- Gtk.builderGetObject builder "c626x6"
          c626x6Entry <- case c626x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c626x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c636x6EntryObj <- Gtk.builderGetObject builder "c636x6"
          c636x6Entry <- case c636x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c636x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c646x6EntryObj <- Gtk.builderGetObject builder "c646x6"
          c646x6Entry <- case c646x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c646x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c656x6EntryObj <- Gtk.builderGetObject builder "c656x6"
          c656x6Entry <- case c656x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c656x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c666x6EntryObj <- Gtk.builderGetObject builder "c666x6"
          c666x6Entry <- case c666x6EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c666x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj

          c117x7EntryObj <- Gtk.builderGetObject builder "c117x7"
          c117x7Entry <- case c117x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c117x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c127x7EntryObj <- Gtk.builderGetObject builder "c127x7"
          c127x7Entry <- case c127x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c127x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c137x7EntryObj <- Gtk.builderGetObject builder "c137x7"
          c137x7Entry <- case c137x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c137x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c147x7EntryObj <- Gtk.builderGetObject builder "c147x7"
          c147x7Entry <- case c147x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c147x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c157x7EntryObj <- Gtk.builderGetObject builder "c157x7"
          c157x7Entry <- case c157x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c157x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c167x7EntryObj <- Gtk.builderGetObject builder "c167x7"
          c167x7Entry <- case c167x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c167x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c177x7EntryObj <- Gtk.builderGetObject builder "c177x7"
          c177x7Entry <- case c177x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c177x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c217x7EntryObj <- Gtk.builderGetObject builder "c217x7"
          c217x7Entry <- case c217x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c217x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c227x7EntryObj <- Gtk.builderGetObject builder "c227x7"
          c227x7Entry <- case c227x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c227x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c237x7EntryObj <- Gtk.builderGetObject builder "c237x7"
          c237x7Entry <- case c237x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c237x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c247x7EntryObj <- Gtk.builderGetObject builder "c247x7"
          c247x7Entry <- case c247x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c247x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c257x7EntryObj <- Gtk.builderGetObject builder "c257x7"
          c257x7Entry <- case c257x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c257x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c267x7EntryObj <- Gtk.builderGetObject builder "c267x7"
          c267x7Entry <- case c267x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c267x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c277x7EntryObj <- Gtk.builderGetObject builder "c277x7"
          c277x7Entry <- case c277x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c277x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c317x7EntryObj <- Gtk.builderGetObject builder "c317x7"
          c317x7Entry <- case c317x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c317x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c327x7EntryObj <- Gtk.builderGetObject builder "c327x7"
          c327x7Entry <- case c327x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c327x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c337x7EntryObj <- Gtk.builderGetObject builder "c337x7"
          c337x7Entry <- case c337x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c337x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c347x7EntryObj <- Gtk.builderGetObject builder "c347x7"
          c347x7Entry <- case c347x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c347x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c357x7EntryObj <- Gtk.builderGetObject builder "c357x7"
          c357x7Entry <- case c357x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c357x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c367x7EntryObj <- Gtk.builderGetObject builder "c367x7"
          c367x7Entry <- case c367x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c367x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c377x7EntryObj <- Gtk.builderGetObject builder "c377x7"
          c377x7Entry <- case c377x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c377x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c417x7EntryObj <- Gtk.builderGetObject builder "c417x7"
          c417x7Entry <- case c417x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c417x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c427x7EntryObj <- Gtk.builderGetObject builder "c427x7"
          c427x7Entry <- case c427x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c427x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c437x7EntryObj <- Gtk.builderGetObject builder "c437x7"
          c437x7Entry <- case c437x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c437x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c447x7EntryObj <- Gtk.builderGetObject builder "c447x7"
          c447x7Entry <- case c447x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c447x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c457x7EntryObj <- Gtk.builderGetObject builder "c457x7"
          c457x7Entry <- case c457x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c457x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c467x7EntryObj <- Gtk.builderGetObject builder "c467x7"
          c467x7Entry <- case c467x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c467x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c477x7EntryObj <- Gtk.builderGetObject builder "c477x7"
          c477x7Entry <- case c477x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c477x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c517x7EntryObj <- Gtk.builderGetObject builder "c517x7"
          c517x7Entry <- case c517x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c517x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c527x7EntryObj <- Gtk.builderGetObject builder "c527x7"
          c527x7Entry <- case c527x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c527x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c537x7EntryObj <- Gtk.builderGetObject builder "c537x7"
          c537x7Entry <- case c537x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c537x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c547x7EntryObj <- Gtk.builderGetObject builder "c547x7"
          c547x7Entry <- case c547x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c547x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c557x7EntryObj <- Gtk.builderGetObject builder "c557x7"
          c557x7Entry <- case c557x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c557x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c567x7EntryObj <- Gtk.builderGetObject builder "c567x7"
          c567x7Entry <- case c567x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c567x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c577x7EntryObj <- Gtk.builderGetObject builder "c577x7"
          c577x7Entry <- case c577x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c577x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c617x7EntryObj <- Gtk.builderGetObject builder "c617x7"
          c617x7Entry <- case c617x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c617x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c627x7EntryObj <- Gtk.builderGetObject builder "c627x7"
          c627x7Entry <- case c627x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c627x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c637x7EntryObj <- Gtk.builderGetObject builder "c637x7"
          c637x7Entry <- case c637x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c637x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c647x7EntryObj <- Gtk.builderGetObject builder "c647x7"
          c647x7Entry <- case c647x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c647x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c657x7EntryObj <- Gtk.builderGetObject builder "c657x7"
          c657x7Entry <- case c657x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c657x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c667x7EntryObj <- Gtk.builderGetObject builder "c667x7"
          c667x7Entry <- case c667x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c667x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c677x7EntryObj <- Gtk.builderGetObject builder "c677x7"
          c677x7Entry <- case c677x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c677x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c717x7EntryObj <- Gtk.builderGetObject builder "c717x7"
          c717x7Entry <- case c717x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c717x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c727x7EntryObj <- Gtk.builderGetObject builder "c727x7"
          c727x7Entry <- case c727x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c727x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c737x7EntryObj <- Gtk.builderGetObject builder "c737x7"
          c737x7Entry <- case c737x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c737x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c747x7EntryObj <- Gtk.builderGetObject builder "c747x7"
          c747x7Entry <- case c747x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c747x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c757x7EntryObj <- Gtk.builderGetObject builder "c757x7"
          c757x7Entry <- case c757x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c757x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c767x7EntryObj <- Gtk.builderGetObject builder "c767x7"
          c767x7Entry <- case c767x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c767x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj
          c777x7EntryObj <- Gtk.builderGetObject builder "c777x7"
          c777x7Entry <- case c777x7EntryObj of
              Nothing -> error "No se pudo encontrar el objeto 'c777x7'"
              Just obj -> Gtk.unsafeCastTo Gtk.Entry obj

          resolver5x5ButtonObj <- Gtk.builderGetObject builder "resolver5x5"
          resolver5x5Button <- case resolver5x5ButtonObj of
              Nothing -> error "No se pudo encontrar el objeto 'resolver5x5'"
              Just obj -> Gtk.unsafeCastTo Gtk.Button obj
        
          resolver6x6ButtonObj <- Gtk.builderGetObject builder "resolver6x6"
          resolver6x6Button <- case resolver6x6ButtonObj of
              Nothing -> error "No se pudo encontrar el objeto 'resolver6x6'"
              Just obj -> Gtk.unsafeCastTo Gtk.Button obj
        
          resolver7x7ButtonObj <- Gtk.builderGetObject builder "resolver7x7"
          resolver7x7Button <- case resolver7x7ButtonObj of
              Nothing -> error "No se pudo encontrar el objeto 'resolver7x7'"
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

              let m1 = [[c115x5, c125x5, c135x5, c145x5, c155x5], 
                      [c215x5, c225x5, c235x5, c245x5, c255x5], 
                      [c315x5, c325x5, c335x5, c345x5, c355x5], 
                      [c415x5, c425x5, c435x5, c445x5, c455x5], 
                      [c515x5, c525x5, c535x5, c545x5, c555x5]]

              comparar m1 [[1,4,4,2,3],[1,4,5,2,3],[1,4,5,2,3],[1,1,5,2,3],[3,3,3,3,3]]

          Gtk.on resolver6x6Button #clicked $ do
              c116x6 <- Gtk.entryGetText c116x6Entry
              c126x6 <- Gtk.entryGetText c126x6Entry
              c136x6 <- Gtk.entryGetText c136x6Entry
              c146x6 <- Gtk.entryGetText c146x6Entry
              c156x6 <- Gtk.entryGetText c156x6Entry
              c166x6 <- Gtk.entryGetText c166x6Entry
              c216x6 <- Gtk.entryGetText c216x6Entry
              c226x6 <- Gtk.entryGetText c226x6Entry
              c236x6 <- Gtk.entryGetText c236x6Entry
              c246x6 <- Gtk.entryGetText c246x6Entry
              c256x6 <- Gtk.entryGetText c256x6Entry
              c266x6 <- Gtk.entryGetText c266x6Entry
              c316x6 <- Gtk.entryGetText c316x6Entry
              c326x6 <- Gtk.entryGetText c326x6Entry
              c336x6 <- Gtk.entryGetText c336x6Entry
              c346x6 <- Gtk.entryGetText c346x6Entry
              c356x6 <- Gtk.entryGetText c356x6Entry
              c366x6 <- Gtk.entryGetText c366x6Entry
              c416x6 <- Gtk.entryGetText c416x6Entry
              c426x6 <- Gtk.entryGetText c426x6Entry
              c436x6 <- Gtk.entryGetText c436x6Entry
              c446x6 <- Gtk.entryGetText c446x6Entry
              c456x6 <- Gtk.entryGetText c456x6Entry
              c466x6 <- Gtk.entryGetText c466x6Entry
              c516x6 <- Gtk.entryGetText c516x6Entry
              c526x6 <- Gtk.entryGetText c526x6Entry
              c536x6 <- Gtk.entryGetText c536x6Entry
              c546x6 <- Gtk.entryGetText c546x6Entry
              c556x6 <- Gtk.entryGetText c556x6Entry
              c566x6 <- Gtk.entryGetText c566x6Entry
              c616x6 <- Gtk.entryGetText c616x6Entry
              c626x6 <- Gtk.entryGetText c626x6Entry
              c636x6 <- Gtk.entryGetText c636x6Entry
              c646x6 <- Gtk.entryGetText c646x6Entry
              c656x6 <- Gtk.entryGetText c656x6Entry
              c666x6 <- Gtk.entryGetText c666x6Entry

              let m2 = [[c116x6, c126x6, c136x6, c146x6, c156x6, c166x6], 
                      [c216x6, c226x6, c236x6, c246x6, c256x6, c266x6], 
                      [c316x6, c326x6, c336x6, c346x6, c356x6, c366x6], 
                      [c416x6, c426x6, c436x6, c446x6, c456x6, c466x6], 
                      [c516x6, c526x6, c536x6, c546x6, c556x6, c566x6],
                      [c616x6, c626x6, c636x6, c646x6, c656x6, c666x6]]

              comparar m2 [[1,1,1,1,3,3],[1,2,2,2,2,3],[1,2,6,4,5,3],[1,2,6,4,5,3],[3,2,5,5,5,3],[3,3,3,3,3,3]]
            
          Gtk.on resolver7x7Button #clicked $ do
            c117x7 <- Gtk.entryGetText c117x7Entry
            c127x7 <- Gtk.entryGetText c127x7Entry
            c137x7 <- Gtk.entryGetText c137x7Entry
            c147x7 <- Gtk.entryGetText c147x7Entry
            c157x7 <- Gtk.entryGetText c157x7Entry
            c167x7 <- Gtk.entryGetText c167x7Entry
            c177x7 <- Gtk.entryGetText c177x7Entry
            c217x7 <- Gtk.entryGetText c217x7Entry
            c227x7 <- Gtk.entryGetText c227x7Entry
            c237x7 <- Gtk.entryGetText c237x7Entry
            c247x7 <- Gtk.entryGetText c247x7Entry
            c257x7 <- Gtk.entryGetText c257x7Entry
            c267x7 <- Gtk.entryGetText c267x7Entry
            c277x7 <- Gtk.entryGetText c277x7Entry
            c317x7 <- Gtk.entryGetText c317x7Entry
            c327x7 <- Gtk.entryGetText c327x7Entry
            c337x7 <- Gtk.entryGetText c337x7Entry
            c347x7 <- Gtk.entryGetText c347x7Entry
            c357x7 <- Gtk.entryGetText c357x7Entry
            c367x7 <- Gtk.entryGetText c367x7Entry
            c377x7 <- Gtk.entryGetText c377x7Entry
            c417x7 <- Gtk.entryGetText c417x7Entry
            c427x7 <- Gtk.entryGetText c427x7Entry
            c437x7 <- Gtk.entryGetText c437x7Entry
            c447x7 <- Gtk.entryGetText c447x7Entry
            c457x7 <- Gtk.entryGetText c457x7Entry
            c467x7 <- Gtk.entryGetText c467x7Entry
            c477x7 <- Gtk.entryGetText c477x7Entry
            c517x7 <- Gtk.entryGetText c517x7Entry
            c527x7 <- Gtk.entryGetText c527x7Entry
            c537x7 <- Gtk.entryGetText c537x7Entry
            c547x7 <- Gtk.entryGetText c547x7Entry
            c557x7 <- Gtk.entryGetText c557x7Entry
            c567x7 <- Gtk.entryGetText c567x7Entry
            c577x7 <- Gtk.entryGetText c577x7Entry
            c617x7 <- Gtk.entryGetText c617x7Entry
            c627x7 <- Gtk.entryGetText c627x7Entry
            c637x7 <- Gtk.entryGetText c637x7Entry
            c647x7 <- Gtk.entryGetText c647x7Entry
            c657x7 <- Gtk.entryGetText c657x7Entry
            c667x7 <- Gtk.entryGetText c667x7Entry
            c677x7 <- Gtk.entryGetText c677x7Entry
            c717x7 <- Gtk.entryGetText c717x7Entry
            c727x7 <- Gtk.entryGetText c727x7Entry
            c737x7 <- Gtk.entryGetText c737x7Entry
            c747x7 <- Gtk.entryGetText c747x7Entry
            c757x7 <- Gtk.entryGetText c757x7Entry
            c767x7 <- Gtk.entryGetText c767x7Entry
            c777x7 <- Gtk.entryGetText c777x7Entry

            let m3 = [[c117x7, c127x7, c137x7, c147x7, c157x7, c167x7, c177x7], 
                    [c217x7, c227x7, c237x7, c247x7, c257x7, c267x7, c277x7], 
                    [c317x7, c327x7, c337x7, c347x7, c357x7, c367x7, c377x7], 
                    [c417x7, c427x7, c437x7, c447x7, c457x7, c467x7, c477x7], 
                    [c517x7, c527x7, c537x7, c547x7, c557x7, c567x7, c577x7],
                    [c617x7, c627x7, c637x7, c647x7, c657x7, c667x7, c677x7],
                    [c717x7, c727x7, c737x7, c747x7, c757x7, c767x7, c777x7]]
                
            comparar m3 [[3,3,3,6,6,6,6],[3,5,5,2,2,2,6],[3,5,4,1,1,2,6],[3,5,4,4,1,2,6],[3,5,1,1,1,5,6],[3,5,5,5,5,5,3],[3,3,3,3,3,3,3]]

           
          Gtk.on window #destroy Gtk.mainQuit

          #showAll window

          Gtk.main