-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g062 where

import LI11920

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0,Movimenta C,Estado {mapaEstado = [[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]] ,jogadoresEstado = [Jogador {pistaJogador = 1,distanciaJogador = 2.05,velocidadeJogador = 0,colaJogador = 5,estadoJogador =  Chao{aceleraJogador = True}}]}),(0,Movimenta C,Estado {mapaEstado = [[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4]],jogadoresEstado = [Jogador {pistaJogador = 1,distanciaJogador = 3.5,velocidadeJogador = 0,colaJogador = 5,estadoJogador =  Chao{aceleraJogador = True}}]}),(0,Movimenta C,Estado {mapaEstado = [[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]],jogadoresEstado = [Jogador {pistaJogador = 1,distanciaJogador = 1,velocidadeJogador = 0,colaJogador = 5,estadoJogador =  Chao{aceleraJogador = True}}]}),(0,Movimenta C,Estado {mapaEstado = [[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]],jogadoresEstado = [Jogador {pistaJogador = 0,distanciaJogador = 1,velocidadeJogador = 0,colaJogador = 5,estadoJogador =  Chao{aceleraJogador = True}}]}),(0,Movimenta B,Estado {mapaEstado = [[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]],jogadoresEstado = [Jogador {pistaJogador = 1,distanciaJogador = 1,velocidadeJogador = 0,colaJogador = 5,estadoJogador =  Morto{timeoutJogador = 1.0 }}]}),(0,Movimenta B,Estado {mapaEstado = [[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]],jogadoresEstado = [Jogador {pistaJogador = 0,distanciaJogador = 2.5,velocidadeJogador = 0,colaJogador = 5,estadoJogador =  Chao{aceleraJogador = True}}]}),(0,Movimenta D,Estado {mapaEstado = [[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]],jogadoresEstado = [Jogador {pistaJogador = 0,distanciaJogador = 1.5,velocidadeJogador = 0,colaJogador = 5,estadoJogador = Ar{alturaJogador = 5,inclinacaoJogador = (-89),gravidadeJogador = 0}}]}),(0,Movimenta E,Estado {mapaEstado = [[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]],jogadoresEstado = [Jogador {pistaJogador = 0,distanciaJogador = 2.5,velocidadeJogador = 0,colaJogador = 5,estadoJogador = Ar{alturaJogador = 5,inclinacaoJogador = 89,gravidadeJogador = 0}}]}),(0,Acelera,Estado {mapaEstado = [[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]],jogadoresEstado = [Jogador {pistaJogador = 0,distanciaJogador = 2.5,velocidadeJogador = 0,colaJogador = 5,estadoJogador = Chao{aceleraJogador = False}}]}),(0,Desacelera,Estado {mapaEstado = [[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]],jogadoresEstado = [Jogador {pistaJogador = 0,distanciaJogador = 2.5,velocidadeJogador = 0,colaJogador = 5,estadoJogador = Chao{aceleraJogador = True}}]}),(0,Dispara,Estado {mapaEstado = [[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]],jogadoresEstado = [Jogador {pistaJogador = 0,distanciaJogador = 2.5,velocidadeJogador = 0,colaJogador = 5,estadoJogador = Chao{aceleraJogador = False}}]}),(0,Dispara,Estado {mapaEstado = [[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 4],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0]],jogadoresEstado = [Jogador {pistaJogador = 0,distanciaJogador = 2.5,velocidadeJogador = 0,colaJogador = 0,estadoJogador = Chao{aceleraJogador = False}}]})]

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada i (Movimenta d) (Estado m js) = Estado m (movimenta i d m js)  
jogada i Acelera (Estado m js) = Estado m (acelerar i js)
jogada i Desacelera (Estado m js) = Estado m (desacelera i js)
jogada i Dispara (Estado m js) = if temCola i js then Estado mN (dispara i js)
                                                 else Estado m js   
   where mN= disparaMapa i js m 

-- | Verifica se o jogador tem cola e não se encontra na 1ª peça da pista, antes de alterar o mapa
temCola :: Int -> [Jogador] -> Bool
temCola i [] = False 
temCola i ((Jogador p d v c e):xs) = if i==0 then if c>0 && d>=1 then True 
                                                                 else False
                                             else temCola (i-1) xs  

-- | Traduz o resultado da jogada 'Movimenta'                                                 
movimenta :: Int -> Direcao -> Mapa -> [Jogador] -> [Jogador]
movimenta _ _ _ [] = []
movimenta i dir m ((Jogador p d v c (Chao a)):xs) | i==0 && dir==C && p/=0 && diferencaAlturasCima<=0.2 = (Jogador (p-1) d v c (Chao a)):xs
                                                  | i==0 && dir==C && p/=0 && diferencaAlturasCima>0.2 && hA<hSC = (Jogador p d v c (Morto 1.0)):xs
                                                  | i==0 && dir==C && p/=0 && diferencaAlturasCima>0.2 && hA>hSC = (Jogador (p-1) d v c (Ar hA iP 0)):xs
                                                  | i==0 && dir==C && p==0 = (Jogador p d v c (Chao a)):xs 
                                                  | i==0 && dir==B && p/=((contaPistas m)-1) && diferencaAlturasBaixo<=0.2 = (Jogador (p+1) d v c (Chao a)):xs
                                                  | i==0 && dir==B && p/=((contaPistas m)-1) && diferencaAlturasBaixo>0.2 && hA<hSB = (Jogador p d v c (Morto 1.0)):xs
                                                  | i==0 && dir==B && p/=((contaPistas m)-1) && diferencaAlturasBaixo>0.2 && hA>hSB = (Jogador (p+1) d v c (Ar hA iP 0)):xs
                                                  | i==0 && dir==B && p==((contaPistas m)-1) = (Jogador p d v c (Chao a)):xs
                                                  | i==0 && dir==D = ((Jogador p d v c (Chao a)):xs)
                                                  | i==0 && dir==E = ((Jogador p d v c (Chao a)):xs) 
                                                  | otherwise = (Jogador p d v c (Chao a)) : movimenta (i-1) dir m xs  
                                                  where diferencaAlturasCima = abs(hA-hSC)
                                                        diferencaAlturasBaixo = abs(hA-hSB) 
                                                        hA = altJogador (Jogador p d v c (Chao a)) m (pecaJogador m (posicaoJogador(Jogador p d v c (Chao a)))) d 
                                                        hSC = altJogador (Jogador (p-1) d v c (Chao a)) m (pecaJogador m (posicaoJogador(Jogador (p-1) d v c (Chao a)))) d 
                                                        hSB = altJogador (Jogador (p+1) d v c (Chao a )) m (pecaJogador m (posicaoJogador(Jogador (p+1) d v c (Chao a)))) d 
                                                        iP = inclinacaoPeca(pecaJogador m (posicaoJogador (Jogador p d v c (Chao a))))
movimenta i dir m ((Jogador p d v c (Ar a ic g)):xs) | i==0 && dir==C = ((Jogador p d v c (Ar a ic g)):xs)
                                                     | i==0 && dir==B = ((Jogador p d v c (Ar a ic g)):xs)
                                                     | i==0 && dir==D && (ic>=(-75)) = (Jogador p d v c (Ar a (ic-15) g)):xs
                                                     | i==0 && dir==D && (ic<(-75)) = (Jogador p d v c (Ar a (-90) g)):xs
                                                     | i==0 && dir==E && (ic<=75) = (Jogador p d v c (Ar a (ic+15) g)):xs
                                                     | i==0 && dir==E && (ic>75) = (Jogador p d v c (Ar a 90 g)):xs
                                                     | otherwise = (Jogador p d v c (Ar a ic g)) : movimenta (i-1) dir m xs 
movimenta i dir m ((Jogador p d v c (Morto t)):xs) = if i==0 then ((Jogador p d v c (Morto t)):xs)
                                                             else (Jogador p d v c (Morto t)) : movimenta (i-1) dir m xs  

-- | Conta o nº de pistas de um mapa 
contaPistas :: Mapa -> Int
contaPistas [] = 0  
contaPistas (p:ps) = 1 + contaPistas ps  

-- | Traduz o resultado da jogada 'Acelera' 
acelerar :: Int -> [Jogador] -> [Jogador] 
acelerar _ [] = []
acelerar i ((Jogador p d v c (Chao a)):xs) = if i==0 then (Jogador p d v c (Chao True)):xs
                                                     else (Jogador p d v c (Chao a)) : acelerar (i-1) xs
acelerar i ((Jogador p d v c e):xs) = if i==0 then (Jogador p d v c e) : xs 
                                              else (Jogador p d v c e) : acelerar (i-1) xs                                                                         

-- | Traduz o resultado da jogada 'Desacelera'
desacelera :: Int -> [Jogador] -> [Jogador]
desacelera _ [] = []
desacelera i ((Jogador p d v c (Chao a)):xs) = if i==0 then (Jogador p d v c (Chao False)):xs
                                                       else (Jogador p d v c (Chao a)) : desacelera (i-1) xs
desacelera i ((Jogador p d v c e):xs) = if i==0 then (Jogador p d v c e) : xs 
                                                else (Jogador p d v c e) : desacelera (i-1) xs  
-- | Traduz o resultado da jogada 'Dispara'
dispara :: Int -> [Jogador] -> [Jogador]
dispara _ [] = []
dispara i ((Jogador p d v c (Chao a)):xs) = if i==0 then (disparaJogador (Jogador p d v c (Chao a))):xs
                                                    else (Jogador p d v c (Chao a)): dispara (i-1) xs
dispara i ((Jogador p d v c e):xs) = if i==0 then (Jogador p d v c e):xs
                                             else (Jogador p d v c e): dispara (i-1) xs   

-- | Verifica se o jogador tem cola e não se encontra na 1ª peça da pista e, caso seja verdade, diminui 1 cola 
disparaJogador :: Jogador -> Jogador
disparaJogador (Jogador p d v c e) = if c>0 && d>=1 then Jogador p d v (c-1) e
                                                    else Jogador p d v c e

-- | Traduz o resultado de disparar, alterando o mapa
disparaMapa :: Int -> [Jogador] -> Mapa -> Mapa
disparaMapa i ((Jogador p d v c e):xs) m = if i==0 then atualizaMapa (p, (fromIntegral(floor d-1))) pD m 
                                                   else disparaMapa (i-1) xs m  
      where pD = pisoDispara(pecaJogador m (posicaoJogador(Jogador p (fromIntegral(floor(d-1))) v c e)))

-- | Altera o piso da peça anterior para Cola 
pisoDispara :: Peca -> Peca 
pisoDispara (Recta piso h ) = Recta Cola h   
pisoDispara (Rampa piso h1 h2 ) = Rampa Cola h1 h2  

-- | Atualiza o mapa com a peça com cola
atualizaMapa :: (Int,Double) -> Peca -> Mapa -> Mapa 
atualizaMapa (0,d) pc (x:xs) = (atualizaPista d pc x):xs
atualizaMapa (p,d) pc (x:xs) = x:(atualizaMapa (p-1,d) pc xs)

-- | Atualiza a pista com a peça com cola
atualizaPista :: Double -> Peca -> Pista -> Pista
atualizaPista d p [] = []
atualizaPista d p (x:xs) = if d==0 then p:xs
                                   else x:atualizaPista (d-1) p xs

-- | Retorna um par de números com a pista e a distância do jogador, respetivamente
posicaoJogador :: Jogador -> (Int,Double)
posicaoJogador (Jogador p d v c e) = (p,d)

-- | Dados um mapa e um par de números com a posição do jogador, retorna a peça onde este se encontra
pecaJogador :: Mapa -> (Int,Double) -> Peca 
pecaJogador ((x:xs):ys) (p,d) = pecaJogadorAcc (0,0) (p,fromIntegral(floor d)) ((x:xs):ys)

pecaJogadorAcc :: (Int,Double) -> (Int,Double) -> Mapa -> Peca 
pecaJogadorAcc (a,b) (p,d) ((x:xs):ys) | a/=p = pecaJogadorAcc (a+1,b) (p,d) ys
                                       | b/=d = pecaJogadorAcc (a,b+1) (p,d) (xs:ys)
                                       | otherwise = x 

-- | Dada uma peça, retorna a sua inclinação
inclinacaoPeca :: Peca -> Double
inclinacaoPeca (Recta _ _) = 0
inclinacaoPeca (Rampa _ a1 a2) = ((atan(fromIntegral(a2-a1)))*180)/pi 

-- | Retorna a altura do jogador numa peça, dependendo da sua distância
altJogador :: Jogador -> Mapa -> Peca -> Double -> Double
altJogador _ _ (Recta _ a) dist = fromIntegral a 
altJogador (Jogador p d v c e) m (Rampa _ a1 a2) dist | dist == fromIntegral(floor dist) = altInicialPeca (pecaJogador m (posicaoJogador (Jogador p d v c e))) 
                                                      | dist /= fromIntegral(floor dist) && a2>a1 && a1==0 = (dist-fromIntegral(floor dist))*fromIntegral(a2-a1)
                                                      | dist /= fromIntegral(floor dist) && a2>a1 && a1/=0 = (dist-fromIntegral(floor dist))*fromIntegral(a2-a1)+(fromIntegral a1)
                                                      | dist /= fromIntegral(floor dist) && a2<a1 && a2==0 = (fromIntegral(ceiling dist)-dist)*fromIntegral(a1-a2)
                                                      | dist /= fromIntegral(floor dist) && a2<a1 && a2/=0 = (fromIntegral(ceiling dist)-dist)*fromIntegral(a1-a2)+(fromIntegral a2) 

-- | Dada uma peça retorna a sua altura inicial 
altInicialPeca :: Peca -> Double
altInicialPeca (Recta _ a) = fromIntegral a 
altInicialPeca (Rampa _ a1 a2) = fromIntegral a1



