algumaCoisa :: Int -> Int -> Int
algumaCoisa a b | a > b = 1
    | a == b = 0
    | a < b = -1

realBattle :: Int -> Character -> Monster -> Int
realBattle moeda char monst | resultado == 1 = 1
    | resultado == 0 && moeda == 1 = 1
    | otherwise = 0
    where resultado = algumaCoisa ((health char) `div` (strength monst)) (( health monst ) `div` ( getStrength char ))