{-battle :: Champion -> Monster -> Int
battle c m = heroAtack (c)  (m)


-}
heroAtack ::Int -> Int -> Int -> Int -> Int
heroAtack a b c d  --CharAtq charVida monsterAtq MonsterVida
	| (b <= 0) = 0
	| otherwise = do
--		putStrLn "Voce causou  de dano ao monstro"
		return (monsterAtack a (b - c) c d)
 

monsterAtack :: Int -> Int -> Int -> Int -> Int
monsterAtack a b c d  --CharAtq charVida monsterAtq MonsterVida
	| (d <= 0) = 1
	| otherwise = do
--		putStrLn "Voce levou  de dano"
		return (heroAtack a (b - c) c d) 