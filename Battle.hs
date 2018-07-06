<<<<<<< HEAD
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
=======
module Battle where
import System.Random;
import Character
import Messages;

gotRight :: String -> Boolean
gotRight "cara" 1 = true
gotRight "coroa" 0 = true
gotRight moeda valor = false

userTurn :: Character -> Monster -> IO()
userTurn char enemy = do
    messages.actionPick

enemyTurn :: Character -> Monster -> IO()
enemyTurn char enemy = do


startBattle :: IO()
startBattle = do
    putStrLn("Escolha 'cara' ou 'coroa': ")
    resp <- getLine
    let won = gotRight rest (randomRIO (0,1))
    messages.coinMessage won
    
main :: IO()
main = do
    userTurn null null
>>>>>>> d71dffea5c2b87bf21fc9add10c7c9dff06ef683
