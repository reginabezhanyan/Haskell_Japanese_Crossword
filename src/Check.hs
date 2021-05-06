-- Модуль для обработок ошибок --
module Check where

import Type
import Field

checkWin :: Field -> String
checkWin f | checkJumble f = "Winning!"
           | otherwise     = "Not yet, try harder."
   
checkNum :: Char -> Bool
checkNum x
    | x == '1' = True
    | x == '0' = True
    | otherwise = False
    
checkStr :: String -> Bool
checkStr x = foldr1 (&&) (map (checkNum) x) 

checkInput :: [String] -> Bool
checkInput x = foldr1 (&&) (map (checkStr) x)

checkMenu :: Maybe FilePath -> FilePath
checkMenu f = case f of
              Nothing -> "field.txt"
              Just fp -> fp