module StoreMessage where

import Character

initialMessage :: IO()
initialMessage = do 
      putStrLn ("Bem vindo a Loja, em que podemos ajudá-lo?");
      
exitText :: IO()
exitText = do
      putStrLn("Obrigado e volte sempre!");
      
armorType :: IO()
armorType = do
      putStrLn("armor");
      
weaponType :: IO()
weaponType = do
      putStrLn("weapon");
      
haveCoins :: Character -> Int
haveCoins character = (coins character)
      
notEnoughMoney :: IO()
notEnoughMoney = do
      putStrLn("Sinto muito, você não me parece ter dinheiro suficiente para comprar isso");

repeteadItem :: IO()
repeteadItem = do
      putStrLn("Você já comprou esse item, escolha novamente");
      
