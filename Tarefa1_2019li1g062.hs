-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g062 where

import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(2,5,1), (2,5,2),(3,4,2),(3,6,3),(2,3,4),(4,4,5),(2,6,3),(2,4,6),(2,1,9)]

-- * Funções pré-definidas da Tarefa 1.

geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.

gera :: Int -> Int -> Int -> Mapa
gera npistas comprimento semente = if comprimento==1 then if npistas>0 then [Recta Terra 0]:gera (npistas-1) comprimento semente
                                                                       else [] 
                                                     else geraPistas al comprimento
     where al=geraPistasAl(juntaAleatorios(geraAleatorios (npistas*comprimento*2 -npistas*2) semente)) (comprimento-1)

-- | Junta os números aleatórios dois a dois
juntaAleatorios :: [Int] -> [(Int,Int)]
juntaAleatorios [] = []
juntaAleatorios (x:y:xs) = (x,y): juntaAleatorios xs

-- | Junta os pares de números aleatórios por pistas 
geraPistasAl :: [(Int,Int)] -> Int -> [[(Int,Int)]]
geraPistasAl [] _ = []
geraPistasAl al comprimento = if ((length al)>0) then take comprimento al : geraPistasAl (drop comprimento al) comprimento   
                                                 else [] 
-- | Gera todas as pistas do mapa
geraPistas :: [[(Int,Int)]] -> Int -> Mapa 
geraPistas _ 0 = [] 
geraPistas [] comprimento = [] 
geraPistas (((a,b):xs):ys) comprimento = (Recta Terra 0 : geraPista ((a,b):xs) (Recta Terra 0) (comprimento-1)) : geraPistas ys comprimento

-- | Gera apenas uma pista
geraPista :: [(Int,Int)] -> Peca -> Int -> Pista 
geraPista [] peca comprimento = []
geraPista ((a,b):xs) peca comprimento = if (comprimento>0) then geraPeca (peca) (a,b) : geraPista xs (geraPeca (peca) (a,b)) (comprimento-1)
                                                           else [] 

-- | Gera uma peça, tendo em conta a peça anterior e o 2º número aleatório
geraPeca :: Peca -> (Int,Int) -> Peca
geraPeca (Recta piso altura) (a,b) | (b>=0 && b<=1) = Rampa (geraPiso piso (a,b)) altura (altura+(b+1))     
                                   | (b>=2 && b<=5) && (altura>0 && (altura-(b-1))>=0) = Rampa (geraPiso piso (a,b)) altura (altura-(b-1))
                                   | (b>=2 && b<=5) && (altura>0 && (altura-(b-1))<0) = Rampa (geraPiso piso (a,b)) altura 0 
                                   | (b>=2 && b<=5) && (altura==0 || (altura==altura-(b-1))) = Recta (geraPiso piso (a,b)) altura
                                   | (b>=6 && b<=9) = Recta (geraPiso piso (a,b)) altura
geraPeca (Rampa piso altura1 altura2) (a,b) | (b>=0 && b<=1) = Rampa (geraPiso piso (a,b)) altura2 (altura2+(b+1))    
                                            | (b>=2 && b<=5) && (altura2>0 && (altura2-(b-1))>=0) = Rampa (geraPiso piso (a,b)) altura2 (altura2-(b-1))
                                            | (b>=2 && b<=5) && (altura2>0 && (altura2-(b-1))<0) = Rampa (geraPiso piso (a,b)) altura2 0 
                                            | (b>=2 && b<=5) && (altura2==0 || (altura2==altura2-(b-1))) = Recta (geraPiso piso (a,b)) altura2
                                            | (b>=6 && b<=9) = Recta (geraPiso piso (a,b)) altura2

-- | Gera o piso, tendo em conta o piso da peça anterior e o 1º número aleatório
geraPiso :: Piso -> (Int,Int) -> Piso 
geraPiso piso (a,b) | (a>=0 && a<=1) = Terra
                    | (a>=2 && a<=3) = Relva
                    | (a==4) = Lama
                    | (a==5) = Boost 
                    | (a>=6 && a<=9) = piso 
   




