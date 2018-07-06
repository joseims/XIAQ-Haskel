module StoreMessage where

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
