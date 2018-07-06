module Messages where
import Character

initialMessage :: Character -> monster -> IO()
initialMessage char enemy = do
    putStrLn("A BATALHA ESTÁ PRESTES A COMEÇAR!!");
    putStrLn("Lembre-se, seus status são: ");
    putStrLn("Força: " ++ show ( getStrength char ));
    putStrLn("Defesa: " ++ show ( getDefense char ));
    putStrLn("Vida: " ++ show ( getHealth char ) ++ "/" ++ show ( getMaxHealth char ));
    putStrLn("Você tem " ++ show ( getPotions char ) ++ " Poções. Use-as sabiamente!!\n");
    -- putStrLn("Você encontrou um " ++ show ( getName enemy ));
    putStrLn("A batalha começou!");

roundStart :: Int -> IO()
roundStart round = do
    putStrLn("Inicia-se o " ++ show (round `div` 2) ++ " round!" );

situation :: Character -> IO()
situation char = do
    putStrLn("Situação do HP: " ++ show (getHealth char ) ++ "/" ++ show (getMaxHealth char) );

enemyDefensive :: IO()
enemyDefensive = do
    putStrLn("O inimigo ficou em posição defensiva");

damageTaken :: Int -> IO()
damageTaken dmg = do
    putStrLn("Você sofreu " ++  show (dmg) ++ " de dano!");

mainDefensive :: IO()
mainDefensive = do
    putStrLn("Você estava na defensiva e bloqueou parte do dano!");

enemySuper :: IO()
enemySuper = do
    putStrLn("O inimigo usou o super ataque!");

noMorePotions :: IO()
noMorePotions = do
    putStrLn("Você não tem mais poções. Tente algo diferente");

usePotion :: IO()
usePotion = do
    putStrLn("Você usou uma poção e recuperou 15 de vida!");

cantSuper :: IO()
cantSuper = do
    putStrLn("Você ainda não pode usar o super ataque.");

actionPick :: IO() -> IO Int
actionPick = do
    putStrLn("Escolha sua ação:");
    putStrLn("[1] Atacar\n[2] Defender\n[3] Super Ataque\n[4] Usar uma poção");
    action <- getLine;
    return (read action :: Int);

coinMessage :: Bool -> String
coinMessage a | a = "Você ganhou o 'cara ou coroa'. A primeira ação é sua!"
    | otherwise = "Você perdeu o 'cara ou coroa'. A primeira ação é do inimigo!"

