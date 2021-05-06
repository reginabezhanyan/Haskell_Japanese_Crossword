-- Меню --
module Menu where

import System.FilePath.Posix
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe
import Graphic
import Type
import DataBase

-- загружаемые файлы (списком)
files :: [String] -> [FilePath]
files strs = map ("./base" </>) strs

-- база данных изменений
filesChange :: [String] -> [FilePath]
filesChange strs = map ("./save/change" </>) strs  

-- получаем менюшку
readMenu :: [FilePath] -> [FilePath] -> Menu
readMenu strs savestrs = Menu
    { games = strs
    , savefile = savestrs
    , selected = Nothing
    , selectedNum = 0
    , widthM = 2 * indent + (1 + (length strs)) * cellSize
    , heightM = 2 * indent + (1 + (length strs)) * cellSize
    , kostyl = 0
    }

-- для выбора файла
selectMenu :: Menu -> String -> Menu
selectMenu m f = m {selected = Just f}

-- взять i-ый файл
chooseMenu :: Menu -> Int -> String
chooseMenu m i = (games m) !! i

-- поменять режимы
changeMenu :: Menu -> Int -> Menu
changeMenu m i = menu{selectedNum = i, kostyl = 1}
                 where menu = selectMenu m (chooseMenu m i)

-- отрисовать меню
drawMenu :: Menu -> Record -> Picture
drawMenu m r = Translate (x) (y) (Pictures [menuLines m, menuNames m, menuTitle m, menuRecordTitle m, menuRecordNames m r,
                                          menuRecordTimeMin m r, menuRecordCol m r, menuRecordTimeSec m r])
           where
           x = - fromIntegral (widthM m)  / 2
           y = - fromIntegral (heightM m) / 2

-- отрисовать линии
menuLines :: Menu -> Picture
menuLines m = Pictures (map (\y -> Line [(i,y), (w-i,y)]) [l*c + i | l <- [0..h]])
            where
            i = fromIntegral indent
            w = fromIntegral (widthM m)
            h = fromIntegral (length (games m))
            c = fromIntegral cellSize

-- отрисовать заголовок
menuTitle :: Menu -> Picture
menuTitle m = Translate (i) (y) (Color (red) (scale compr compr (Text "Japanese crosswords")))
          where
          y = fromIntegral ((heightM m) - indent - cellSize + 5)
          i = fromIntegral (indent * 2)
          compr = 0.12
          
-- отрисовать названия
menuNames :: Menu -> Picture
menuNames m = Pictures (zipWith3 (Translate) x y (map (scale compr compr . Text) txt))
            where
            x = [i | l <- [1..h]]
            y = [i + l*c | l <- [0..h-1]]
            txt = map (takeBaseName) (games m)
            h = fromIntegral (length (games m))
            i = fromIntegral (indent + 5)
            c = fromIntegral cellSize
            compr = 0.15

          
-- отрисовать заголовок рекордов
menuRecordTitle :: Menu -> Picture
menuRecordTitle m = Translate (i) (y) (Color (red) (scale compr compr (Text "Records")))
          where
          y = 500
          i = 500 + fromIntegral (indent * 2)
          compr = 0.2
          
takeName :: (String, Int) -> String
takeName (name, time) = name

-- отрисовать названия рекордов
menuRecordNames :: Menu -> Record -> Picture
menuRecordNames m r = Pictures (zipWith3 (Translate) x y (map (scale compr compr . Text) txt))
            where
            x = [500 | l <- [1..h]]
            y = [500 - c*h + l*c | l <- [0..h-1]]
            txt = map (takeName) r
            h = fromIntegral (length txt)
            i = fromIntegral (indent + 5)
            c = fromIntegral cellSize
            compr = 0.15   

takeTimeMin :: (String, Int) -> String
takeTimeMin (name, time) = show (time `div` 60)

takeTimeSec :: (String, Int) -> String
takeTimeSec (name, time) = show (time `mod` 60)

takeCol :: (String, Int) -> String
takeCol (name, time) = ":"

-- отрисовать время рекордов - минуты
menuRecordTimeMin :: Menu -> Record -> Picture
menuRecordTimeMin m r = Pictures (zipWith3 (Translate) x y (map (scale compr compr . Text) time))
            where
            x = [600 | l <- [1..h]]
            y = [500 - c*h + l*c | l <- [0..h-1]]
            time = map (takeTimeMin) r
            h = fromIntegral (length time)
            i = fromIntegral (indent + 5)
            c = fromIntegral cellSize
            compr = 0.15  

-- отрисовать время - секунды
menuRecordTimeSec :: Menu -> Record -> Picture
menuRecordTimeSec m r = Pictures (zipWith3 (Translate) x y (map (scale compr compr . Text) time))
            where
            x = [630 | l <- [1..h]]
            y = [500 - c*h + l*c | l <- [0..h-1]]
            time = map (takeTimeSec) r
            h = fromIntegral (length time)
            i = fromIntegral (indent + 5)
            c = fromIntegral cellSize
            compr = 0.15 

-- отрисовать время рекордов - разделитель
menuRecordCol :: Menu -> Record -> Picture
menuRecordCol m r = Pictures (zipWith3 (Translate) x y (map (scale compr compr . Text) time))
            where
            x = [620 | l <- [1..h]]
            y = [500 - c*h + l*c | l <- [0..h-1]]
            time = map (takeCol) r
            h = fromIntegral (length time)
            i = fromIntegral (indent + 5)
            c = fromIntegral cellSize
            compr = 0.15             
         
-- удалить меню
delMenu :: Menu -> Picture
delMenu m = Color (white) (Polygon [(0,  0), (0,  h), (h, h), (h, 0)])
          where h = fromIntegral (heightM m)

-- убрать костыль
reduce :: Menu -> Menu
reduce m = m {kostyl = 0}