data Monster = Monster { 
    strength :: Int,
    defense :: Int,
    health :: Int,
    name :: String
} deriving (Show)



rand :: Int
rand = 0


randMonsterName ::  String
randArmorName = ["Ladão","Dragão","Golem","Gosma", "Vampiro","Lobisomen","Rato gigante"]) !! rand 




randMonster:: Monster
randArmor = Monster randStatus randStatus randStatus (randArmorName++randAlterer)


randStatus :: Int
randStatus = (15 + rand)

randAlterer :: String
randAlterer =  [" Pacifista"," Burro"," Cego"," Imaginário"," Sem Pernas", " Invisivel", " Gigantesco"," de 3 cabeças", " Aterrorizante"," Cabeludo"," Rochoso"," Assassino", ""] !! rand
