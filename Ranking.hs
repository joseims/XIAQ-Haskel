module Ranking
( 
insert
) where

import Data.Sequence
import Data.List

-- Adicionar no array, tupla com nome e pontuacao, array com o ranking
addInRank :: (String, Int) -> [(String, Int)] -> [(String, Int)]
addInRank name_points my_rank = my_rank ++ [name_points]

-- Ordenando Rank
orderedRank :: [(String, Int)] -> [(String, Int)]
orderedRank rank = sortOn snd rank 

-- Selecionar as três ultimas pontuacoes
getThreeLargest :: [(String, Int)] -> [(String, Int)]
getThreeLargest rank = take 3 $ reverse rank

-- Inserir a pontuacao no rank mapeado  
insert :: (String, Int) -> [(String, Int)] -> [(String, Int)]
insert name_points rank = getThreeLargest $ orderedRank $ addInRank name_points rank 
