module DataBase where

import Text.Read 
import Type
import Field
import Check
 

--сохранение сессии по нажатию s 

--выбор нужного файла
choosefiles :: [String] -> Int -> String
choosefiles strs i = strs !! i

--сохранение новой сессии
saveBoard :: Field -> [String] -> IO Field
saveBoard f strs = do
        writeFile (choosefiles strs (numberfield f)) (show f)
        return(f)
        
--если ничего не нашли, то загружаем игру по новой из файла                   
--поиск нужной сессии 
searchBoard :: String -> String -> IO Field
searchBoard fch fb =
    case readMaybe fch of
        Just a -> return(a)
        Nothing -> return(readField (lines fb))
 
--получаем текущий список рекордов
scaneRecord :: String -> Record
scaneRecord str = 
    case readMaybe str of
        Just a -> a
        Nothing -> []

addRecord :: Int -> String -> Record -> Record
addRecord new_time name_field (r:rs) | name_field == (fst r) && new_time < (snd r) = (fst r, new_time) : rs 
                                     | name_field == (fst r) && new_time >= (snd r) = r : rs 
                                     | otherwise = r : addRecord new_time name_field rs
addRecord new_time name_field [] = (name_field, new_time) : []
           
--сохранение нового рекорда в таблицу рекордов
saveRecord :: Int -> String -> App -> Record -> IO App
saveRecord x name a record = do
                    writeFile "./save/record.txt" (show (addRecord x name record)) 
                    return(a)

    
    