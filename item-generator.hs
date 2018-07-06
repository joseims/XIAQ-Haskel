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
    alterer <- randAlterer
    return (Item (randStatus strRand 10 gpm) (randStatus hpRand 10 (gpm -1 )) 1 (randStatus price 10 gpm) (fullName name alterer))

fullName :: String -> String -> String 
fullName name alt = name++alt


randStatus ::  Int -> Int -> Int -> Int
randStatus rando n  gpm = (n + rando) + (((n + rando)*gpm) `div` 5)

randAlterer :: IO String
randAlterer = do
    i <- rand 8
    return ([" Furioso", " Brilhante"," Resistente"," Lendario"," Irreparavel"," Fraco"," Sujo"," Macio",""] !! i)
