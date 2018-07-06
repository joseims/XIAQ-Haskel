{-Item Generator-}
import System.IO
import System.Directory
import System.Random

data Item = Item { 
    strength_ :: Int,
    health_ :: Int,
    type_ :: Int,
    price :: Int,
    name :: String
    } deriving (Show)


randWeaponName :: IO String
randWeaponName = do 
    i <- rand 3
    return (["Espada","Bastao", "Manopla","Espada e escudo"] !! i)



rand ::Int -> IO Int
rand n = randomRIO (0,n)


randArmorName :: IO String
randArmorName = do
    i <- rand 4
    return  ( ["Armadura Leve","Armadura Media", "Armadura Pesada","Armadura Espinhosa","Kimono"] !! i)
 


randArmor :: Int -> IO Item
randArmor gpm = do
    strRand <- rand 20
    hpRand <- rand 20
    price <- rand 15
    name <- randArmorName
    alterer <- randAlterer
    return (Item (randStatus strRand 10 gpm) (randStatus hpRand 10 (gpm -1 )) 0 (randStatus price 10 gpm) (fullName name alterer))

randWeapon :: Int -> IO Item
randWeapon gpm = do
    strRand <- rand 20
    hpRand <- rand 20
    price <- rand 15
    name <- randWeaponName
    alterer <- randAltererItem
    return (Item (randStatus strRand 10 gpm) (randStatus hpRand 10 (gpm -1 )) 1 (randStatus price 10 gpm) (fullName name alterer))

fullName :: String -> String -> String 
fullName name alt = name++alt

getInitialWeapon :: Item
getInitialWeapon =  (Item (20) (20) 1 0 "weapon")


getInitialArmor :: Item
getInitialArmor =  (Item (20) (20) 1 0 "armor")

randStatus ::  Int -> Int -> Int -> Int
randStatus rando n  gpm = (n + rando) + (((n + rando)*gpm) `div` 5)

randAltererItem :: IO String
randAltererItem = do
    i <- rand 8
    return ([" Furioso", " Brilhante"," Resistente"," Lendario"," Irreparavel"," Fraco"," Sujo"," Macio",""] !! i)

    
{-Monster Generator-}

data Monster = Monster { 
    _strength_ :: Int,
    _health_ :: Int,
    _name_ :: String
} deriving (Show)

randMonsterName :: IO String
randMonsterName = do 
    i <- rand 6
    return (["Ladrao","Dragao","Golem","Gosma", "Vampiro","Lobisomen","Rato gigante"] !! i)


monsterFullName :: IO String 
monsterFullName = do
    name <- randMonsterName
    alterer <- randAlterer
    return (name++alterer) 

randMonster:: Int -> IO Monster
randMonster gpm = do
    strRand <- rand 30
    hpRand <- rand 15
    price <- rand 15
    name <- monsterFullName 
    return (Monster (randStatus strRand 10 gpm) (randStatus hpRand 7 gpm) name)

randAlterer :: IO String
randAlterer = do
    i <- rand 12
    return ([" Pacifista"," Burro"," Cego"," Imaginario"," Sem Pernas", " Invisivel", " Gigantesco"," de 3 cabecas", " Aterrorizante"," Cabeludo"," Rochoso"," Assassino", ""] !! i)

{-Character-}

data Attributes = Attributes { 
                            strength :: Int,
                            health :: Int
                             } deriving (Show)

data Character = Character {
                            attributes :: Attributes,
                            weapon :: Item,
                            armor :: Item, 
                            gpm :: Int,
                            coins :: Int
                            } deriving (Show)


getStrength :: Character -> Int
getStrength character = (strength (attributes character))

getHealth :: Character -> Int
getHealth character = (health (attributes character))

getWeapon :: Character -> Item
getWeapon character = (weapon character)

getArmor :: Character -> Item
getArmor character = (weapon character)

getGPM :: Character -> Int
getGPM character = (gpm character)

getCoins :: Character -> Int
getCoins character = (coins character)

setItems :: Item -> Item -> Attributes
setItems weapon armor = (Attributes 
                                ((strength_ weapon) + (strength_ armor)) 
                                ((health_ weapon) + (health_ armor))
                               )

setCharacter :: Item -> Item -> Int -> Int -> Character
setCharacter weapon armor gpm coins = (Character (setItems weapon armor) weapon armor gpm 0)

addGPM :: Character -> Int -> Character
addGPM char gpm_ = (Character (attributes char) (getWeapon char) (getArmor char) ((getGPM char) + gpm_) (getCoins char))

addCoins :: Character -> Int -> Character
addCoins char coins = (Character (attributes char) (getWeapon char) (getArmor char) (getGPM char) ((getCoins char) + coins))

equipArmor :: Character -> Item -> Character
equipArmor char armor = (Character (setItems (getWeapon char) armor) (getWeapon char) armor (getGPM char) (getCoins char))

equipWeapon :: Character -> Item -> Character
equipWeapon char weapon = (Character (setItems weapon (getArmor char)) weapon (getArmor char) (getGPM char) (getCoins char))

{-Battle-}

gotRight :: String -> Int
gotRight "cara" 1 = 1
gotRight "coroa" 0 = 1
gotRight moeda valor = 0

startBattle :: IO Int
startBattle = do
    putStrLn("Escolha 'cara' ou 'coroa': ")
    input <- getLine
    return gotRight input

battleMoeda :: Int -> Int -> Int -> Int -> Int -> IO Int
battleMoeda 0 healthChar healthMonster attackChar attackMonster = battleM healthChar healthMonster attackChar attackMonster
battleMoeda 1 healthChar healthMonster attackChar attackMonster = battleH healthChar healthMonster attackChar attackMonster

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
    where resultado = battleMoeda moeda (health char) ( _health_ monst ) ( getStrength char ) (_strength_ monst)

battleMain :: Character -> IO Int
battleMain char = do
    putStrLn("A batalha está prestes a começar")
    monst <- randMonster (getGPM char)
    return realBattle startBattle char monst

{-Store-}

addCoinsAndGPM :: Character -> Int -> Int -> Character
addCoinsAndGPM char coins gpm_ = (Character (attributes char) (getWeapon char) (getArmor char) ((getGPM char) + gpm_) ((getCoins char) + coins))




{-Game-}

ranking :: IO ()
ranking = do
    getRanking 0
    

getRanking :: Int -> IO ()
getRanking 0 = putStrLn("Ranking do facil") --chamar a funcao do ranking
getRanking 1 = putStrLn("Ranking do medio") --chamar a funcao do ranking
getRanking 2 = putStrLn("Ranking do dificil") -- chamar a funcao do rankigns
-- --chama menu dnv


game :: Character -> Int -> Int -> Int -> IO()
game char 5 points difficult = toStoreOrNot char points difficult
game char n pontuacao difficult
    | wonBattle = do 
        char1 <- (addCoinsAndGPM 5 1)
        coins
        game char n+1 pontuacao+5
    | otherwise = saveRecord difficult pontuacao
    where wonBattle = battleMain char

createHero :: Int -> Character
createHero n = do
    gpm <- getDifficultGPM n
    weapon <- getInitialWeapon
    armor <- getInitialArmor
    return (setCharacter weapon armor gpm)

toStoreOrNot :: Int -> Int -> Character -> IO()
toStoreOrNot points difficult char = do
    wannaGoStore
    ans <- getLine
    if ((read ans) == 1) then return (goToStore char points difficult) else return ((game char 0 points difficult))


saveRecord :: Int -> Int -> IO()
saveRecord difficult points  = do
    --saveRank difficult points
    return main


goToStore :: Int -> Int -> Character -> IO()
goToStore char points difficult = do
    return (game char 0 points difficult)


selectOption :: Int -> IO()
selectOption 1 = startGame
selectOption 2 = ranking
selectOption n = exit

exit :: IO()
exit = return

selectDifficult :: IO Int
selectDifficult = do
    difficultText
    option <- getLine  
    return (read option)

getDifficultGPM :: Int -> Int
getDifficultGPM 1 = 1
getDifficultGPM 2 = 3
getDifficultGPM n = 5


startGame :: IO()
startGame = do
    diff <- selectDifficult
    char <- (createHero diff)
    startGameText
    return (game char 0 0 diff)

{-Log-}

save content = do
  fileHandle <- openFile "log.txt" ReadWriteMode
  currentContent <- hGetContents fileHandle
  writeFile "log2.txt" ((currentContent) ++ content)
  hClose fileHandle
  renameFile "log2.txt" "log.txt"

{-Ranking-}


{-Game Messages-}

wannaGoStore :: IO ()
wannaGoStore = do
    putStrLn("Escolha uma opção:")
    putStrLn("1 - Ir para loja")
    putStrLn("2 - Continuar batalhando")

welcome :: IO()
welcome = do
    putStrLn ("Bem Vindo ao XIAQ")
    putStrLn("Escolha uma opção:")
    putStrLn("1 - Começar o Jogo")
    putStrLn("2 - Ver Ranking")
    putStrLn("3 - Sair")

difficultText :: IO()
difficultText = do
    putStrLn("Selecione uma dificuldade")
    putStrLn("1 - Facil")
    putStrLn("2 - Medio")
    putStrLn("3 - Dificil")

woncoins :: IO()
woncoins = do 
    putStrLn("Você ganhou 5 moedas!")

startGameText :: IO()
startGameText = do
    putStrLn("Prepare-se  o jogo vai começar!")








main :: IO() 
main = do
    welcome
    option <- getLine  
    return (selectOption (read option)) 