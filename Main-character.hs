data Attributes = Attributes { strength :: Int,
					 		   health :: Int,
							   maxHealth :: Int 
							 } deriving (Show)

data Character = Character { attributes :: Attributes,
							 weapon :: Item,
							 armor :: Item, 
							 gpm :: Int,
							 coins :: Int,
						   } deriving (Show)


getStrength :: Character -> Int
getStrength character = (strength (attributes character))

getDefense :: Character -> Int
getDefense character = (defense (attributes character))

getHealth :: Character -> Int
getHealth character = (health (attributes character))

getMaxHealth :: Character -> Int
getMaxHealth character = (maxHealth (attributes character))

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
								((strength' weapon) + (strength' armor)) 
								((health' weapon) + (health' armor))
								((maxHealth' weapon) + (maxHealth' armor))
							   )

setCharacter :: Item -> Item -> Int -> Int -> Character
setInitialCharacter weapon armor gpm coins potions = (Character (setItems weapon armor) weapon armor gpm 0)

addGPM :: Character -> Int -> Character
addGPM char gpm_ = (Character (attributes char) (getWeapon char) (getArmor char) ((getGPM char) + gpm_) (getCoins char))

addCoins :: Character -> Int -> Character
addCoins char coins = (Character (attributes char) (getWeapon char) (getArmor char) (getGPM char) ((getCoins char) + coins))

equipArmor :: Character -> Item -> Character
equipArmor char armor = (Character (setItems (getWeapon char) armor) (getWeapon char) armor (getGPM char) (getCoins char))

equipWeapon :: Character -> Item -> Character
equipWeapon char weapon = (Character (setItems weapon (getArmor char)) weapon (getArmor char) (getGPM char) (getCoins char))