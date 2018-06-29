module ranking
( 
name, 
points,
level
)

where

import Data.Sequence
import qualified Data.Map as Map
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
insert :: (String, Int) -> Int -> [(Int, [(String, Int)])] -> [(Int, [(String, Int)])]
insert name_points level rank | level == 1 then getThreeLargest $ orderedRank $ addInRank name_points rank 
                              | level == 2 then getThreeLargest $ orderedRank $ addInRank name_points rank
                              | level == 3 then getThreeLargest $ orderedRank $ addInRank name_points rank
