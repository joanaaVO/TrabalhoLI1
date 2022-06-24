-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g062 where

import LI11920
import Tarefa2_2019li1g062
import Tarefa4_2019li1g062

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot i (Estado m js) | checkMovimentaCima i (Estado m js) = Just (Movimenta C) 
                    | checkMovimentaDireita i (Estado m js) = Just (Movimenta D)
                    | checkMovimentaBaixo i (Estado m js) = Just (Movimenta B)
--                    | Just Movimenta E = undefined
                    | checkAcelera i js = Just Acelera 
--                    | Just Desacelera = undefined               
                    | checkDispara i js = Just Dispara                    
                    | otherwise = Nothing 

-- | Função que dado o identificador do robot e um estado, retorna um 'Bool' relativo à necessidade de se movimentar para cima
checkMovimentaCima :: Int -> Estado -> Bool
checkMovimentaCima i (Estado m []) = False
checkMovimentaCima i (Estado m ((Jogador _ _ _ _ (Ar _ _ _)):xs)) = if i==0 then False 
                                                                            else checkMovimentaCima (i-1) (Estado m xs)
checkMovimentaCima i (Estado m ((Jogador _ _ _ _ (Morto _)):xs)) = if i==0 then False 
                                                                           else checkMovimentaCima (i-1) (Estado m xs)
checkMovimentaCima i (Estado m ((Jogador p d v c (Chao a)):xs)) | i==0 && p==0 = False
                                                                | i==0 && p/=0 && diferencaAlturasCima > 0.2 = False
                                                                | i==0 && p/=0 && diferencaAlturasCima <= 0.2 && (pisoPecaCima == Lama || pisoPecaCima == Cola) = False
                                                                | i==0 && p/=0 && diferencaAlturasCima <= 0.2 && pisoPecaCima == Boost && (pisoPecaCimaSeguinte == Lama || pisoPecaCimaSeguinte == Cola) = False
                                                                | i==0 && p/=0 && diferencaAlturasCima <= 0.2 && pisoPecaCima == Boost && (pisoPecaCimaSeguinte /= Lama || pisoPecaCimaSeguinte /= Cola) = True 
                                                                | i==0 && p/=0 && diferencaAlturasCima <= 0.2 && (pisoPecaCima == Terra || pisoPecaCima == Relva) && (pisoPecaCimaSeguinte == Boost) = True 
                                                                | i==0 && p/=0 && diferencaAlturasCima <= 0.2 && (pisoPecaCima == Terra || pisoPecaCima == Relva) && (pisoPecaCimaSeguinte /= Boost) = False
                                                                | otherwise = checkMovimentaCima (i-1) (Estado m xs)  
                                                                where diferencaAlturasCima= abs (hA-hS)
                                                                      hA = altJogador (Jogador p d v c (Chao a)) m (pecaJogador m (posicaoJogador(Jogador p d v c (Chao a)))) d
                                                                      hS = altJogador (Jogador (p-1) d v c (Chao a)) m (pecaJogador m (posicaoJogador(Jogador (p-1) d v c (Chao a)))) d 
                                                                      pisoPecaCima = pisoPeca(pecaJogador m (posicaoJogador (Jogador (p-1) d v c (Chao a))))
                                                                      pisoPecaCimaSeguinte = pisoPeca(pecaJogador m (posicaoJogador (Jogador (p-1) (d+1) v c (Chao a))))

checkMovimentaDireita :: Int -> Estado -> Bool
checkMovimentaDireita i (Estado m []) = False 
checkMovimentaDireita i (Estado m ((Jogador _ _ _ _ (Chao _)):xs)) = if i==0 then False 
                                                                             else checkMovimentaDireita (i-1) (Estado m xs)
checkMovimentaDireita i (Estado m ((Jogador _ _ _ _ (Morto _)):xs)) = if i==0 then False
                                                                              else checkMovimentaDireita (i-1) (Estado m xs)
--checkMovimentaDireita i (Estado m ((Jogador _ _ _ _ (Ar a i g)):xs)) | i==0 && 

-- | Função que dado o identificador do robot e um estado, retorna um 'Bool' relativo à necessidade de se movimentar para baixo
checkMovimentaBaixo :: Int -> Estado -> Bool 
checkMovimentaBaixo i (Estado m []) = False 
checkMovimentaBaixo i (Estado m ((Jogador _ _ _ _ (Ar _ _ _)):xs)) = if i==0 then False 
                                                                             else checkMovimentaBaixo (i-1) (Estado m xs)
checkMovimentaBaixo i (Estado m ((Jogador _ _ _ _ (Morto _)):xs)) = if i==0 then False 
                                                                            else checkMovimentaBaixo (i-1) (Estado m xs)
checkMovimentaBaixo i (Estado m ((Jogador p d v c (Chao a)):xs)) | i==0 && p==((contaPistas m)-1) = False
                                                                 | i==0 && p/=((contaPistas m)-1) && diferencaAlturasBaixo > 0.2 = False
                                                                 | i==0 && p/=((contaPistas m)-1) && diferencaAlturasBaixo <= 0.2 && (pisoPecaBaixo == Lama || pisoPecaBaixo == Cola) = False
                                                                 | i==0 && p/=((contaPistas m)-1) && diferencaAlturasBaixo <= 0.2 && pisoPecaBaixo == Boost && (pisoPecaBaixoSeguinte == Lama || pisoPecaBaixoSeguinte == Cola) = False
                                                                 | i==0 && p/=((contaPistas m)-1) && diferencaAlturasBaixo <= 0.2 && pisoPecaBaixo == Boost && (pisoPecaBaixoSeguinte /= Lama || pisoPecaBaixoSeguinte /= Cola) = True
                                                                 | i==0 && p/=((contaPistas m)-1) && diferencaAlturasBaixo <= 0.2 && (pisoPecaBaixo == Terra || pisoPecaBaixo == Relva) && (pisoPecaBaixoSeguinte == Boost) = True 
                                                                 | i==0 && p/=((contaPistas m)-1) && diferencaAlturasBaixo <= 0.2 && (pisoPecaBaixo == Terra || pisoPecaBaixo == Relva) && (pisoPecaBaixoSeguinte /= Boost) = False
                                                                 | otherwise = checkMovimentaBaixo (i-1) (Estado m xs)  
                                                                 where diferencaAlturasBaixo= abs (hA-hS)
                                                                       hA = altJogador (Jogador p d v c (Chao a)) m (pecaJogador m (posicaoJogador(Jogador p d v c (Chao a)))) d
                                                                       hS = altJogador (Jogador (p+1) d v c (Chao a)) m (pecaJogador m (posicaoJogador(Jogador (p+1) d v c (Chao a)))) d
                                                                       pisoPecaBaixo = pisoPeca(pecaJogador m (posicaoJogador (Jogador (p+1) d v c (Chao a))))
                                                                       pisoPecaBaixoSeguinte = pisoPeca(pecaJogador m (posicaoJogador (Jogador (p+1) (d+1) v c (Chao a)))) 

-- | Dada uma peça retorna o seu piso
pisoPeca :: Peca -> Piso 
pisoPeca (Recta piso _) = piso 
pisoPeca (Rampa piso _ _) = piso

-- | Função que dado o identificador do robot e uma lista de jogadores, retorna um 'Bool' relativo à necessidade de acelerar
checkAcelera :: Int -> [Jogador] -> Bool 
checkAcelera i [] = False
checkAcelera i ((Jogador p d v c (Chao _)):xs) = if i==0 then True else checkAcelera (i-1) xs 
checkAcelera i ((Jogador p d v c e):xs) = if i==0 then False else checkAcelera (i-1) xs

-- | Dada uma lista de jogadores, retorna as suas posições 
getPosicaoJogadores :: [Jogador] -> [(Int,Double)]
getPosicaoJogadores [] = []
getPosicaoJogadores (x:xs) = posicaoJogador x : getPosicaoJogadores xs 

-- | Função que dado o identificador do robot e uma lista de jogadores, retorna um 'Bool' relativo à necessidade de disparar 
checkDispara :: Int -> [Jogador] -> Bool
checkDispara i [] = False 
checkDispara i ((Jogador p d v c (Morto _)):xs) = if i==0 then False else checkDispara (i-1) xs 
checkDispara i ((Jogador p d v c (Ar _ _ _)):xs) = if i==0 then False else checkDispara (i-1) xs   
checkDispara i ((Jogador p d v c e):xs) | i==0 && d<1 = False
                                        | i==0 && c==0 = False
                                        | i==0 && d>1 && c>0 && jogadorPecasAnt == False = False
                                        | i==0 && d>1 && c>0 && jogadorPecasAnt = True 
                                        | otherwise = checkDispara (i-1) xs  
                                        where jogadorPecasAnt = checkJogadorLastPecas (posicaoJogador (Jogador p d v c e)) (getPosicaoJogadores ((Jogador p d v c e):xs))

-- | Dada a posição do robot e a lista de posições dos restantes jogadores, a função retorna um 'Bool' relativo à existência de jogadores nas duas peças anteriores à do robot
checkJogadorLastPecas :: (Int,Double) -> [(Int,Double)] -> Bool 
checkJogadorLastPecas _ [] = False 
checkJogadorLastPecas (p,d) ((x,y):xs) = if (x,fromIntegral(floor y)) == (p,fromIntegral(floor d-1)) || (x,fromIntegral(floor y)) == (p,fromIntegral(floor d-2)) then True 
                                                                                                                                                                 else checkJogadorLastPecas (p,d) xs  