-- ranking :: IO ()
-- ranking = do
--     --Selecione o nivel do ranking
--     --getRaking nivel
    

-- getRanking :: Int -> IO ()
-- getRanking 0 = putStraLn("Ranking do facil") --chamar a funcao do ranking
-- getRanking 1 = putStrLn("Ranking do medio") --chamar a funcao do ranking
-- getRanking 2 = putStrLn("Ranking do dificil") -- chamar a funcao do rankigns
-- --chama menu dnv


game :: Character ->  Int -> Int -> Int ->  IO()
game char 5 points difficult= toStoreOrNot char point difficult
game char n pontuacao
    |wonBattle = do 
        char1 <- (addCoinsAndGPM 5 1)
        coins
        game char n+1 pontuacao+5
    |otherwise = saveRecord difficult points
    where wonBattle = --metodo da batalha



createHero ::Int -> Character
createHero n = do
    gpm <- getDifficultGPM n
    weapon <- getInitialWeapon
    armor <- getInitialArmor
    return (setCharacter weapon armor gpm)

toStoreOrNot ::Int -> Int -> Character -> IO()
toStoreOrNot points difficult char = do
    wannaGoStore
    ans <- getLine
    return if ((read ans) == 1) then (goToStore char points difficult) else ((game char 0 points difficult))


saveRecord :: Int -> Int -> IO()
saveRecord difficult points  = do
    --saveRank difficult points
    return menu


goToStore ::Int -> Int -> Character -> IO()
goToStore char points difficult = do
    char2 <- --metodoDaLoja
    return (game char2 0 points difficult)


menu :: IO() 
menu = do
    welcome
    option <- getLine  
    return (selectOption (read option)) 


selectOption :: Int -> IO()
selectOption 1 = startGame
selectOption 2 = ranking
selectOption n = exit


selectDifficult :: IO Int
selectDifficult = do
    difficultText
    option <- getLine  
    return (read option)

getDifficultGPM :: Int -> Int
getDifficultGMP 1 = 1
getDifficultGMP 2 = 3
getDifficultGMP n = 5


startGame :: IO()
startGame = do
    diff <- selectDifficult
    char <- (createHero diff)
    startGameText
    return (game char 0 0 diff)

exit :: IO()
return 
