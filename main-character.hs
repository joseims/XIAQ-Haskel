data Item = Item { strength_ :: Int,
			 	   defense_ :: Int,
		 		   health_ :: Int,
				   maxHealth_ :: Int 
				 } deriving (Show)

data Attributes = Attributes { strength :: Int,
						 	   defense :: Int,
					 		   health :: Int,
							   maxHealth :: Int 
							 } deriving (Show)

data Character = Character { attributes :: Attributes,
							 weapon :: Item,
							 armor :: Item, 
							 gpm :: Int,
							 coins :: Int,
							 potions :: Int
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

getPotions :: Character -> Int
getPotions character = (potions character)


setCharacter :: Attributes -> Item -> Item -> Int -> Int -> Int -> Character
setCharacter attributes weapon armor gpm coins potions = 
	Character attributes weapon armor gpm coins potions

setInitialItems :: Item -> Item -> Attributes
setInitialItems weapon armor = (Attributes 
								((strength' weapon) + (strength' armor)) 
								((defense' weapon) + (defense' armor)) 
								((health' weapon) + (health' armor))
								((maxHealth' weapon) + (maxHealth' armor))
							   )

setInitialCharacter :: Item -> Item -> Int -> Int -> Int -> Character
setInitialCharacter weapon armor gpm coins potions = (setCharacter (setInitialItems weapon armor) weapon armor gpm coins potions)
