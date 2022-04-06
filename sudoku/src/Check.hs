module Check where


checkSym :: Char -> Bool 
checkSym a 
    | a == '1' = True 
    | a == '2' = True 
    | a == '3' = True 
    | a == '4' = True 
    | a == '5' = True 
    | a == '6' = True 
    | a == '7' = True 
    | a == '8' = True 
    | a == '9' = True 
    | a == '0' = True 
    | otherwise = False 

checkColumn :: String -> Bool 
checkColumn a = and (map (checkSym) a)

checkConf :: [String] -> Bool 
checkConf a = and (map (checkColumn) a)