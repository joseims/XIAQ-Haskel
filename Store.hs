import Item
import System.Random
import StoreMessage

module Store where

text = [("armor"), ("weapon"), ("Poção (Recupera 15 de vida)"), ("Bem vindo a Loja, em que podemos ajudá-lo?"), ("Você já comprou esse item, escolha novamente"), ("Obrigado e volte sempre!"), ("Sinto muito, você não me parece ter dinheiro suficiente para comprar isso"), ("Opção Inválida, escolha novamente")]

-- Mensagem
printMenuStore :: String
printMenuStore = do
    StoreMessage.initialMessage

-- Rand que representa o preço aleatorio
rand :: Int
rand = 5

-- Verifica se possui moedas suficiente
checkCoins :: Int -> Bool
checkCoins x = if x > rand then True else False

buyItem :: Int -> Item
buyItem x = if x > rand then 
