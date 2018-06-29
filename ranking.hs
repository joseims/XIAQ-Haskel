module ranking
( 
nome, 
pontuacao
)

where

import Data.Sequence
import qualified Data.Map as Map

insert :: (String, Int) -> Int -> [(String, Int)] -> [(String, Int)]
insert name&points dificulty rank | dificulty == 1 = 

ranking_easy = []
ranking_medium = []
ranking_hard = []

