intFlip :: String -> Int
intFlip "coroa" = 0
intFlip "cara" = 1

startBattle :: IO Int
startBattle = do
    putStrLn("Escolha 'cara' ou 'coroa': ")
	entr <- getLine
    return intFlip entr