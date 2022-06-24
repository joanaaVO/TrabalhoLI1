-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g062 where

import LI11920
import Tarefa0_2019li1g062
import Tarefa2_2019li1g062

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(1,[[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]], Jogador {pistaJogador = 1,distanciaJogador = 2.05,velocidadeJogador = 0,colaJogador = 5,estadoJogador =  Chao{aceleraJogador = True}})] 

-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)

-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t m (Jogador p d v c (Chao a)) = Jogador p d (novaVelocidade t m (Jogador p d v c (Chao a))) c (Chao a)  
acelera t m (Jogador p d v c (Ar a i g)) = Jogador p d ((novaVelocidadeAr t v) + (velocidadeGravidade t g)) c (Ar a i g) 


novaVelocidade :: Double -> Mapa -> Jogador -> Double 
novaVelocidade t m (Jogador p d v c e) = if va>=0 then va 
                                                  else 0   
   where va = v + (accelMota - atrito (pecaJogador m (posicaoJogador (Jogador p d v c e))) *v) * t
            where accelMota = if v<2 && (e==Chao True) then 1 else 0 

atrito :: Peca -> Double
atrito (Recta p i) = case p of 
                   Terra -> 0.25
                   Relva -> 0.75
                   Lama -> 1.50
                   Boost -> -0.50
                   Cola -> 3.00
atrito (Rampa p i1 i2) = case p of 
                       Terra -> 0.25
                       Relva -> 0.75
                       Lama -> 1.50
                       Boost -> -0.50
                       Cola -> 3.00

novaVelocidadeAr :: Double -> Double -> Double 
novaVelocidadeAr t va = if (va - (0.125*va*t))>=0 then va - (0.125*va*t)
                                                  else 0 

velocidadeGravidade :: Double -> Double -> Double  
velocidadeGravidade t g = if (g+1*t)>= 0 then g+1*t
                                         else 0   

-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t m (Jogador p d v c (Morto to)) | (to-t)>0 = Jogador p d v c (Morto t)
                                      | (to-t)<0 = Jogador p d 0 c (Chao False)
move t m (Jogador p d v c (Chao a)) | (d+v*t) < limPeca = Jogador p (d+v*t) v c (Chao a)
                                    | (d+v*t) >= limPeca && iS>=iA = Jogador p (fromIntegral(floor(d+v*t))) v c (Chao a)
                                    | (d+v*t) >= limPeca && iS< iA = Jogador p (fromIntegral(floor(d+v*t))) v c (Ar h iA 0)
                                    where iS = inclinacaoPeca(pecaJogador m (posicaoJogador (Jogador p (d+1) v c (Chao a))))
                                          iA = inclinacaoPeca(pecaJogador m (posicaoJogador (Jogador p d v c (Chao a)))) 
                                          h = fromIntegral(alturaPecaJogador(pecaJogador m (posicaoJogador (Jogador p d v c (Chao a)))))
                                          limPeca = limitePeca m (Jogador p d v c (Chao a)) (pecaJogador m (posicaoJogador(Jogador p d v c (Chao a))))                                              
move t m (Jogador p d v c (Ar a i g)) | (intersetam jogador vetor) == False && (d+v*t) < limPeca = Jogador p (d+v*t) v c (Ar hA i g)
                                      | (intersetam jogador vetor) && difInclinacoes < 45 = Jogador p pI v c (Chao False) 
                                      | (intersetam jogador vetor) && difInclinacoes >= 45 = Jogador p pI 0 c (Morto 1.0)
                                      | (intersetam jogador vetor) == False && (d+v*t) >= limPeca = Jogador p (fromIntegral(floor(d+v*t))) v c (Ar hA i g)    
                                      where hJ = altJogador (Jogador p d v c (Ar a i g)) m (pecaJogador m (posicaoJogador(Jogador p d v c (Ar a i g)))) (d+v*t)
                                            hA = altJogador (Jogador p d v c (Ar a i g)) m (pecaJogador m (posicaoJogador(Jogador p d v c (Ar a i g)))) (fromIntegral(floor(d+v*t))) + (a-altJogador (Jogador p d v c (Ar a i g)) m (pecaJogador m (posicaoJogador(Jogador p d v c (Ar a i g)))) d)
                                            difInclinacoes = i-inclinacaoPeca(pecaJogador m (posicaoJogador (Jogador p d v c (Ar a i g))))
                                            jogador = ((Cartesiano (fromIntegral(floor d)) a1),(Cartesiano (fromIntegral(floor d+1)) a2))
                                            a1 = altInicialPeca(pecaJogador m (posicaoJogador(Jogador p d v c (Ar a i g))))
                                            a2 = altFinalPeca(pecaJogador m (posicaoJogador(Jogador p d v c (Ar a i g))))
                                            vetor = ((Cartesiano d a),(Cartesiano (d-(sin (i*pi/180))*(v*t)) (a-(cos (i*pi/180))*(v*t))))
                                            pI = pontoIntersecao (intersecao jogador vetor) 
                                            limPeca = limitePeca m (Jogador p d v c (Ar a i g)) (pecaJogador m (posicaoJogador(Jogador p d v c (Ar a i g))))

pontoIntersecao :: Ponto -> Double
pontoIntersecao (Cartesiano ta tb) = ta 

alturaPecaJogador :: Peca -> Int
alturaPecaJogador (Recta _ a) = a
alturaPecaJogador (Rampa _ a1 a2) | a1<a2 = a2-a1
                                  | a1>a2 = a1-a2 

altFinalPeca :: Peca -> Double
altFinalPeca (Recta _ a) = fromIntegral a 
altFinalPeca (Rampa _ a1 a2) = fromIntegral a2 

limitePeca :: Mapa -> Jogador -> Peca -> Double 
limitePeca m (Jogador p d v c e) (Recta _ _) = fromIntegral (floor d) + 1    
limitePeca m (Jogador p d v c e) (Rampa _ _ _) = fromIntegral (floor d) + sqrt (((fromIntegral alt)^2)+1) 
                        where alt = alturaPecaJogador(pecaJogador m (posicaoJogador (Jogador p d v c e)))

