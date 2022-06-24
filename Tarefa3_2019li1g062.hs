-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g062 where

import LI11920
import Data.List 

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [[[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]],[[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4]]]

-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
desconstroi :: Mapa -> Instrucoes
desconstroi [] = []
desconstroi (x:xs) = auxDescontroi 0 (x:xs)  
  where auxDescontroi :: Int -> Mapa -> Instrucoes
        auxDescontroi i [] = []
        auxDescontroi i (x:xs) = padroesHorizontais (juntaInstrucoes (saltaFstPeca i x)) ++ auxDescontroi (i+1) xs   

-- | Desconstroi uma pista
desconstroiPista :: Int -> Pista -> Instrucoes
desconstroiPista i [] = []
desconstroiPista i (x:xs) = desconstroiPeca i x : desconstroiPista i xs

saltaFstPeca :: Int -> Pista -> Instrucoes
saltaFstPeca i [] = []
saltaFstPeca i (x:y:xs) = desconstroiPista i (y:xs) 

-- | Desconstroi uma peça
desconstroiPeca :: Int -> Peca -> Instrucao  
desconstroiPeca i p = case p of 
                    Recta piso h -> Anda [i] piso 
                    Rampa piso h1 h2 -> if (h1<h2) then Sobe [i] piso (h2-h1) 
                                                   else Desce [i] piso (h1-h2)

juntaInstrucoes :: Instrucoes -> [Instrucoes]
juntaInstrucoes [] = [[]]
juntaInstrucoes (a:as) = aux [a] as 
    where aux x [] = [x]
          aux x (y:ys) = if elem y x then aux (y:x) ys 
                                     else x:aux [y] ys

padroesHorizontais :: [Instrucoes] -> Instrucoes
padroesHorizontais [] = []
padroesHorizontais ([x]:xs) = x : padroesHorizontais xs 
padroesHorizontais ((x:xs):ys) = (Repete (contaInstrucoes (x:xs)) [x]) : padroesHorizontais ys  

contaInstrucoes :: Instrucoes -> Int 
contaInstrucoes [] = 0 
contaInstrucoes (x:xs) = 1 + contaInstrucoes xs   


 




