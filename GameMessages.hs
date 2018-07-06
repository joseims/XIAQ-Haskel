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

coins :: IO()
coins = do 
    putStrLn("Você ganhou 5 moedas!")

startGameText :: IO()
startGameText = do
    putStrLn("Prepare-se  o jogo vai começar!")


