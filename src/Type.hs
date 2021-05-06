 -- Типы данных --
module Type where

data State = Filled | Pointed | Empty deriving (Show, Eq, Read) -- состояние ячейки (закрашена или пустая)

data Mode = Fill | Point deriving (Show, Eq, Read) -- ставим в поле точки или закрашиваем
convertMode :: Mode -> State
convertMode Fill = Filled
convertMode Point = Pointed

data Cell = Cell {current :: State, expected :: State} deriving (Show, Read) -- ячейка (текущее, правильное)

type Grid = [[Cell]] -- игровое поле - матрица из ячеек
 
-- игровое поле
data Field = Field 
    { gamegrid :: Grid -- игровая сетка
    , jumble :: Int -- количество беспорядков
    , width :: Int -- ширина игрового поля в клетках
    , height :: Int -- высота игрового поля в клетках 
    , horline :: [[Int]] -- цифры сбоку от сетки
    , verline :: [[Int]] -- цифры сверху от сетки
    , mode :: Mode -- режим
    , timer :: Int -- таймер
    , regime :: Int -- смена режимов в приложении
    , numberfield :: Int --номер текущего поля
    } deriving (Show, Read)
    
-- менюшка
data Menu = Menu
    { games :: [String] -- список названий файлов
    , savefile :: [String] -- список названий файлов на сохранение игры
    , selected :: Maybe String -- выбранная головоломка
    , selectedNum :: Int --номервыбранной головоломки
    , widthM :: Int -- ширина
    , heightM :: Int -- высота
    , kostyl :: Int -- остановка работы
    }    
    
-- приложение
data App = App {menu :: Menu, field :: Field, err :: Bool}

--таблица рекордов
type Record = [(String, Int)] 