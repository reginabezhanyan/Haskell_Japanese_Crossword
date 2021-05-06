  -- Это main --

import Gameplay

main :: IO ()
main = run


--  filecontent <- readFile "field.txt"
--  if ((checkInput (lines filecontent)) == False) then do
--        putStrLn "Error"
--     else do
--        let board = readField (lines filecontent)
--        print "Horizontal lines:"
--        print (horline board)
--        print "Vertical lines:"
--        print (verline board)
--        print (checkWin (changeField (changeField (changeField (changeField (changeField (changeField (changeField (changeField board 0 1) 0 2) 1 0) 1 2) 2 0) 2 1) 2 2) 0 0))

