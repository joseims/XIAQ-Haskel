
battleMoeda :: Int -> Int -> Int -> Int -> Int -> IO Int
battleMoeda 0 = battleM (health char) ( health monst ) ( getStrength char ) (strength monst)
battleMoeda 1 = battleH (health char) ( health monst ) ( getStrength char ) (strength monst)

battleM :: Int -> Int -> Int -> Int -> IO Int
battleM hphero hpmonster attackhero attackmonster = do

    if (hpmonster > 0) then putStrLn( "Você recebeu " ++  show (attackmonster) ++ " de dano") else return 1

    if (hphero > 0) then putStrLn( "Você causou " ++ show (attackhero) ++ " de dano" ) else return -1

    return battleM (hphero - attackmonster) (hpmonster - attackhero) attackhero attackmonster

battleH :: Int -> Int -> Int -> Int -> IO Int
battleH hphero hpmonster attackhero attackmonster = do

    if (hphero > 0) then putStrLn( "Você causou " ++ show (attackhero) ++ " de dano" ) else return -1

    if (hpmonster > 0) then putStrLn( "Você recebeu " ++  show (attackmonster) ++ " de dano") else return 1

    return battleH (hphero - attackmonster) (hpmonster - attackhero) attackhero attackmonster


realBattle :: Int -> Character -> Monster -> Int
realBattle moeda char monst | resultado == 1 = 1
    | resultado == 0 && moeda == 1 = 1
    | otherwise = 0
    where resultado = battleMoeda moeda (health char) ( health monst ) ( getStrength char ) (strength monst)