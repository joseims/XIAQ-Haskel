import System.Random
import System.IO.Unsafe

data Item = Item { 
	strength_ :: Int,
	defense_ :: Int,
	health_ :: Int,
	type_ :: Int,
	name :: String
} deriving (Show)


randWeaponName ::  [Char]
randWeaponName =  (["Espada","Maça", "Manopla","Espada e escudo"] !! rand)



rand :: Int
rand = 3


randArmorName :: String
randArmorName = do
    a <-  ( ["Armadura Leve","Armadura Media", "Armadura Pesada","Armadura Espinhosa","Kimono"] !! rand)
    return a



randArmor :: Item
randArmor = Item randStatus randStatus randStatus 0 (randArmorName++randAlterer)


randWeapon :: Item
randWeapon = Item randStatus randStatus randStatus 1 (randArmorName++randAlterer)


randStatus :: Int
randStatus = (15 + rand)

randAlterer :: String
randAlterer = do
    a <- ([" Furioso", " Brilhante"," Resistente"," Lendario"," Irreparavel"," Fraco"," Sujo"," Macio",""] !! rand )
    return  a
