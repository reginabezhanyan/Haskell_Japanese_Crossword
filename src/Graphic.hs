-- Отрисовка игрового поля --
module Graphic where

import Graphics.Gloss.Interface.Pure.Game
import Type
import Field

-- размер клетки в пикселях
cellSize :: Int
cellSize = 30

-- отступ в пискселях
indent :: Int
indent = 10

-- отступ под таймер в пикселях
timerIndent :: Int
timerIndent = 30

-- размер поля с числами
lineSize :: Field -> (Field -> [[Int]]) -> Int
lineSize f x = maximum ( map length (x f))

-- пересчитать размер в клетках
getSize :: Int -> Int
getSize x = x * cellSize

-- ширина экрана в пикселях
screenWidth :: Field -> Int
screenWidth f = getSize (width f + lineSize f horline) + 2*indent

-- высота экрана в пикселях
screenHeight :: Field -> Int
screenHeight f = getSize (height f + lineSize f verline) + 2*indent + timerIndent

-- отрисовать поле
drawGame :: Field -> Picture
drawGame f = Translate (x) (y) (Pictures [drawGrid f, drawLines f, drawNums f, drawMode f, drawCell f, drawTimer f, drawWin f])
           where
           x = - fromIntegral (screenWidth f)  / 2
           y = - fromIntegral (screenHeight f) / 2

-- отрисовать сетку
drawGrid :: Field -> Picture
drawGrid f = Pictures (hl ++ vl)
           where
           hl = map (\y -> Line [(i,y), (i+dx,y)]) [l*c + j | l <- [0..h]]
           vl = map (\x -> Line [(x,j), (x,dy+j)]) [l*c + i | l <- [0..w]]
           i = fromIntegral (indent + (getSize(lineSize f horline)))
           j = fromIntegral indent
           dx = fromIntegral (getSize(width f))
           dy = fromIntegral (getSize(height f))
           w = fromIntegral (width f)
           h = fromIntegral (height f)
           c = fromIntegral cellSize

-- отрисовать поля для чисел
drawLines :: Field -> Picture
drawLines f = Pictures (hl ++ vl)
           where
           hl = map (\y -> Line [(i,y), (i+dx,y)]) [l*c + i | l <- [0..h]]
           vl = map (\x -> Line [(x,i+h*c), (x,dy+i+h*c)]) [l*c + i + dx| l <- [0..w]]
           dx = fromIntegral (getSize(lineSize f horline))
           dy = fromIntegral (getSize(lineSize f verline))
           w = fromIntegral (width f)
           h = fromIntegral (height f)
           c = fromIntegral cellSize
           i = fromIntegral indent

-- отрисовать числа
drawNums :: Field -> Picture
drawNums f = Pictures [hl, vl]
           where
           hl = drawNum (makeX wh 1 hor) (makeY h (-c) hor) (makeList hor)
           vl = drawNum (makeY w c ver) (makeX wv (-1) ver) (makeList ver)
           wh = fromIntegral (getSize(lineSize f horline -1)) + i
           wv = fromIntegral (getSize (height f)) + i
           hor = horline f   
           ver = verline f
           h = fromIntegral (getSize (height f -1))
           w = fromIntegral (getSize (lineSize f horline))
           i = fromIntegral (indent) * 1.5
           c = fromIntegral cellSize
           
           makeList :: [[Int]] -> [Int]
           makeList [] = []
           makeList x = head x ++ makeList (tail x)
           
makeX :: Float -> Float -> [[Int]] -> [Float]
makeX _ _ [] = []
makeX m k x = [m - k*(l - j)*c | j <- [1..l]] ++ (makeX m k (tail x))
          where
          l = fromIntegral (length (head x))
          c = fromIntegral cellSize

makeY :: Float -> Float -> [[Int]] -> [Float]
makeY _ _ [] = []
makeY h c x = replicate l (i + h) ++ (makeY (h+c) c (tail x))
          where
          l = length (head x)
          i = fromIntegral (indent) * 1.5

-- нарисовать цифры по координатам
drawNum :: [Float] -> [Float] -> [Int] -> Picture
drawNum x y l = Pictures (zipWith3 (Translate) x y (map (scale compr compr . Text . show) l))
              where compr = 0.12
             
-- отображение режима
drawMode :: Field -> Picture
drawMode f | mode f == Fill = Pictures [(Color (greyN 0.5) (Polygon [(a, b), (a,  b+c), (a+c, b+c), (a+c, b)])), (drawMark a b (convertMode (mode f)))]
           | mode f == Point = Pictures [(Color (greyN 0.5) (Polygon [(a, b), (a,  b+c), (a+c, b+c), (a+c, b)])), (drawMark a b (convertMode (mode f)))]
           where
           a = fromIntegral (indent)
           b = fromIntegral ((screenHeight f) - (indent + timerIndent)) - c
           c = fromIntegral (cellSize)

-- закрасить клетку
drawMark :: Float -> Float -> State -> Picture
drawMark x y m | m == Filled = Translate (x) (y) (Polygon [(1,  2), (1,  a+1), (a, a+1), (a, 2)])
               | m == Pointed = Translate (x) (y) (Pictures [(drawMark 0 0 Empty), (Line [(1, 2), (a, a+1)]), (Line [(1, a+1), (a, 2)])])
               | m == Empty = Translate (x) (y) (Color (white) (Polygon [(1, 2), (1,  a+1), (a, a+1), (a, 2)]))
               where a = fromIntegral (cellSize - 2)

-- закрашивание игрового поля             
drawCell :: Field -> Picture
drawCell f = pictures drawCells
           where
           drawCells = foldMap draw1 (zip [0..] (gamegrid f))
           draw1 (j, linecell) = map draw2 (zip [0..] linecell)
             where
             draw2 (i, cell) = drawMark (fromIntegral (indent + (getSize(lineSize f horline)) + (getSize i))) (fromIntegral (indent + (getSize ((height f) - j - 1)))) (current cell)

-- отрисовка таймера
drawTimer :: Field -> Picture
drawTimer f = Pictures [new, time]
              where
              x = fromIntegral (screenWidth f)
              y = fromIntegral ((screenHeight f) - indent - timerIndent)
              a = fromIntegral (timerIndent)
              i = fromIntegral (indent)
              t =  show (fst (getTime f)) ++ ":" ++ show (snd (getTime f))
              compr = 0.15
              new = Translate (0) (y) (Color (white) (Polygon [(0,  0), (0,  a), (x, a), (x, 0)]))
              time = Translate (i) (y+i) (scale compr compr (Text t))

-- надпись о победе
drawWin :: Field -> Picture
drawWin f | jumble f == 0 = Pictures [table, title] 
          | otherwise = Blank
            where
              table = Color (greyN 0.9) (Translate (x-10) (y-5) (Polygon [(0,  0), (0,  50), (230, 50), (230, 0)]))
              title = Color (red) (Translate x y (scale compr compr (Text "WINNING!")))
              x = fromIntegral (screenWidth f) / 2 - 80
              y = fromIntegral (screenHeight f) / 2 - 20
              compr = 0.4

-- удалить поле
delField :: Field -> Picture
delField f = Color (white) (Polygon [(0,  0), (0,  h), (w, h), (w, 0)])
          where
          h = fromIntegral (screenHeight f)
          w = fromIntegral (screenWidth f)