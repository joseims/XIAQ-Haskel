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