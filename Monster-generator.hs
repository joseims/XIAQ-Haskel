import System.Random

data Monster = Monster { 
    strength :: Int,
    health :: Int,
    name :: String
} deriving (Show)


rand ::Int -> IO Int
rand n = randomRIO (0,n)


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


randStatus :: Int -> Int -> Int -> Int
randStatus rando n  gpm = (n + rando) + (((n + rando)*gpm) `div` 5)

randAlterer :: IO String
randAlterer = do
    i <- rand 12
    return ([" Pacifista"," Burro"," Cego"," Imaginario"," Sem Pernas", " Invisivel", " Gigantesco"," de 3 cabecas", " Aterrorizante"," Cabeludo"," Rochoso"," Assassino", ""] !! i)
