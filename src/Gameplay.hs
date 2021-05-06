-- Игровой процесс --  
module Gameplay where
import Graphics.Gloss.Interface.IO.Game 
import System.IO.Unsafe 
import System.Exit
import System.FilePath.Posix
import System.Directory
import System.IO
import Type
import Field
import Check
import Graphic 
import Menu
import DataBase

-- файл с головоломкой
filePath :: Menu -> FilePath
filePath m = (checkMenu (selected m))

-- обновить поле (поменять таймер)
fieldUpdate :: Float -> Field -> Field
fieldUpdate _ f = (nextSec f)

-- заглушка, реагирует только на событие
menuUpdate :: Float -> Menu -> Menu
menuUpdate _ m = m

-- изменение состояния поля
handleGame :: Event -> Field -> [String] -> IO Field 
handleGame (EventKey (SpecialKey KeySpace) Down _ _) f strs = return(changeMode f)                                                        
handleGame (EventKey (MouseButton LeftButton) Down _ mouse) f strs | x >= 0 && x < width f && y >= 0 && y < height f = return(changeField f y x)
                                                                  | otherwise = return(f)
                                                                     where
                                                                       x = mouseToCoordX mouse f
                                                                       y = mouseToCoordY mouse f
handleGame (EventKey (Char 'q') Down _ _) f strs = return(f{regime = 3})
handleGame (EventKey (Char 's') Down _ _) f strs = saveBoard f strs
handleGame (EventKey (SpecialKey KeyEsc) Down _ _) f strs = exitSuccess
handleGame _ f _ = return(f)  


-- Получить координаты клетки под мышкой
mouseToCoordX :: Point -> Field -> Int
mouseToCoordX (x, y) f = (floor (x + fromIntegral (screenWidth f) / 2) - (indent + getSize(lineSize f horline))) `div` cellSize

mouseToCoordY :: Point -> Field -> Int
mouseToCoordY (x, y) f = (height f) - (floor (y + fromIntegral (screenHeight f) / 2) - indent) `div` cellSize -1

-- изменение состояния меню
handleMenu :: Event -> Menu -> IO Menu 
handleMenu (EventKey (MouseButton LeftButton) Down _ mouse) m | y >= 0 && y < h = return(changeMenu m y)
                                                              | otherwise = return(m)
                                                                where
                                                                   h = length (games m)
                                                                   y = mouseToNum mouse m 
handleMenu _ m = return(m) 

mouseToNum :: Point -> Menu -> Int
mouseToNum (_, y) m = ((floor (y + fromIntegral (heightM m) / 2) - indent)) `div` cellSize

makeApp :: Menu -> App
makeApp m = App {menu = m, field =makeField, err = False}

-- отрисовка
drawApp :: App -> IO Picture
drawApp a = do 
    handle <- readFile "./save/record.txt"
    if err a then do return(drawErr)
      else if kostyl (menu a) == 1 then do return(delMenu (menu a))
        else if regime (field a) == 2 || regime (field a) == 3 then do return(Pictures [delField (field a), drawMenu (menu a) (scaneRecord handle)])
          else do
            case (selected (menu a)) of
                Nothing -> return(drawMenu (menu a) (scaneRecord handle))
                Just fp -> return(drawGame (field a))

-- обработка
changeReg :: Event -> IO App -> IO App
changeReg (EventKey (MouseButton LeftButton) Down _ _) a = makeFregime a
changeReg _ a = a 

handleEvent :: Event -> App -> IO App
handleEvent eve a = do
    if kostyl (menu a) == 1 then do makeMswitch a
      else if regime (field a) == 1 then do changeReg eve (checkFR a)
        else if regime (field a) == 3 then do makeFregime (return(a))
          else do
            m <- handleMenu eve (menu a)
            f <- handleGame eve (field a) (savefile (menu a))
            case (selected (menu a)) of
                 Nothing -> return(a {menu = m})
                 Just fp -> return(a {field = f})
                     
-- обновление
appUpdate :: Float -> App -> IO App
appUpdate f a | regime (field a) == 1 = return(a)
              | otherwise = case (selected (menu a)) of
                                Nothing -> return(a{menu = menuUpdate f(menu a)})
                                Just fp -> return(a{field = fieldUpdate f (field a)})

--сохранение рекорда в таблицу рекордов (если он таковым являлся)                         
checkFR :: App -> IO App
checkFR a = do 
        handle <- openFile "./save/record.txt" ReadMode
        str <- hGetLine handle
        hClose handle
        case (selected (menu a)) of
            Nothing -> return(a)
            Just name -> saveRecord (timer (field a)) (takeBaseName name) a (scaneRecord str)

-- смена режимов работы приложения
makeMswitch :: App -> IO App
makeMswitch a = do
        createDirectoryIfMissing True $ takeDirectory (choosefiles (savefile (menu a)) (selectedNum (menu a)))
        appendFile (choosefiles (savefile (menu a)) (selectedNum (menu a))) ""
        fch <- readFile (choosefiles (savefile (menu a)) (selectedNum (menu a)))
        fb <- readFile (filePath (menu a))
        board <- searchBoard fch fb
        return(a{field = board{numberfield = selectedNum (menu a)}, menu = (reduce (menu a)), err = False})
                   
makeFregime :: IO App -> IO App
makeFregime aa = do
          a <- aa
          return(a{field = f, menu = readMenu (games (menu a)) (savefile (menu a)), err = False})
            where f = makeField {regime = 2}

-- надпись об ошибке
drawErr :: Picture
drawErr = Pictures [table, title]
          where
          table = Color (greyN 0.9) (Polygon [(-100,  -25), (-100,  25), (100, 25), (100, -25)])
          title = Color (red) (scale compr compr (Text "ERROR!"))
          compr = 0.4

-- программа, что запускает игру и отрисовку поля
run :: IO ()
run = do
    file <- listDirectory "./base"
    let app = makeApp (readMenu (files file) (filesChange file)) 
    playIO FullScreen white 1 app drawApp handleEvent appUpdate