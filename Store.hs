module Store where

import Main-character
import System.Random
import StoreMessage

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

-- Compra armadura
buyArmor :: Character -> Item -> Character
buyArmor character item =  
    | checkCoins = Main-character.equipArmor character item
    | otherwise = character, StoreMessage.notEnoughMoney

-- Compra arma
buyWeapon :: Character -> Item -> Character
buyWeapon character item =  
    | checkCoins = Main-character.equipWeapon character item
    | otherwise = character, StoreMessage.notEnoughMoney
