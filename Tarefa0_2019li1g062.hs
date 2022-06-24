-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g062 where

-- * Funções não-recursivas.

-- | Um ponto a duas dimensões dado num referencial cartesiado (distâncias aos eixos vertical e horizontal)
--
-- <<http://li1.lsd.di.uminho.pt/images/cartesiano.png cartesisano>>
-- , ou num referencial polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
--
-- <<http://li1.lsd.di.uminho.pt/images/polar.png polar>>
data Ponto = Cartesiano Double Double | Polar Double Angulo deriving Show

-- | Um ângulo em graus.
type Angulo = Double

-- ** Funções sobre vetores

-- | Um 'Vetor' na representação escalar é um 'Ponto' em relação à origem.
type Vetor = Ponto
-- ^ <<http://li1.lsd.di.uminho.pt/images/vetor.png vetor>>

-- *** Funções gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x1+x2) (y1+y2)
somaVetores v1 v2 = somaVetores (polar2cart v1) (polar2cart v2)

polar2cart c@(Cartesiano x y) = c -- igual a retornar Cartesiano x y
polar2cart (Polar r a) = Cartesiano (r * cos a) (r * sin a)

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (Cartesiano x1 y1) (Cartesiano x2 y2) = Cartesiano (x2-x1) (y2-y1)
subtraiVetores v1 v2 = subtraiVetores (polar2cart v1) (polar2cart v2)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Double -> Vetor -> Vetor
multiplicaVetor a (Cartesiano x1 y1) = Cartesiano (a*x1) (a*y1)
multiplicaVetor a v1 = multiplicaVetor a (polar2cart v1)

-- ** Funções sobre rectas.

-- | Um segmento de reta é definido por dois pontos.
type Reta = (Ponto,Ponto)

-- | Testar se dois segmentos de reta se intersetam.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersetam :: Reta -> Reta -> Bool
intersetam ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) = (0<=ta && ta<=1) && (0<=tb && tb<=1) 
               where ta = ((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3))/((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
                     tb = ((y1-y2)*(x1-x3)+(x2-x1)*(y1-y3))/((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
intersetam (v1,v2) (v3,v4) = intersetam ((polar2cart v1),(polar2cart v2)) ((polar2cart v3),(polar2cart v4))

-- | Calcular o ponto de intersecao entre dois segmentos de reta.
--
-- __NB:__ Aplique as equações matemáticas bem conhecidas, como explicado por exemplo em <http://www.cs.swan.ac.uk/~cssimon/line_intersection.html>.
intersecao :: Reta -> Reta -> Ponto
intersecao ((Cartesiano x1 y1),(Cartesiano x2 y2)) ((Cartesiano x3 y3),(Cartesiano x4 y4)) = (Cartesiano ta tb)
              where ta = ((y3-y4)*(x1-x3)+(x4-x3)*(y1-y3))/((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
                    tb = ((y1-y2)*(x1-x3)+(x2-x1)*(y1-y3))/((x4-x3)*(y1-y2)-(x1-x2)*(y4-y3))
intersecao (v1,v2) (v3,v4) = intersecao ((polar2cart v1),(polar2cart v2)) ((polar2cart v3),(polar2cart v4)) 
-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
--
-- __Sugestão:__ use a função 'length' que calcula tamanhos de listas
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido _ [] = False    
eIndiceListaValido n l = n < (length l) 

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | A dimensão de um mapa dada como um par (/número de linhas/,/número de colunhas/).
type DimensaoMatriz = (Int,Int)

-- | Uma posição numa matriz dada como um par (/linha/,/colunha/).
-- As coordenadas são dois números naturais e começam com (0,0) no canto superior esquerdo, com as linhas incrementando para baixo e as colunas incrementando para a direita:
--
-- <<http://li1.lsd.di.uminho.pt/images/posicaomatriz.png posicaomatriz>>
type PosicaoMatriz = (Int,Int)

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
--
-- __Sugestão:__ relembre a função 'length', referida anteriormente.
dimensaoMatriz :: Matriz a -> DimensaoMatriz
dimensaoMatriz [[]] = (0,0)
dimensaoMatriz ((x:xs):ys) = ((1+ length ys), 1+ length xs)

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool 
ePosicaoMatrizValida (a,b) [[]] = False
ePosicaoMatrizValida (a,b) ((x:xs):ys) = a<=length ys && b<=length xs

-- * Funções recursivas.

-- ** Funções sobre ângulos

-- | Normaliza um ângulo na gama [0..360).
--  Um ângulo pode ser usado para representar a rotação
--  que um objecto efectua. Normalizar um ângulo na gama [0..360)
--  consiste, intuitivamente, em extrair a orientação do
--  objecto que resulta da aplicação de uma rotação. Por exemplo, é verdade que:
--
-- prop> normalizaAngulo 360 = 0
-- prop> normalizaAngulo 390 = 30
-- prop> normalizaAngulo 720 = 0
-- prop> normalizaAngulo (-30) = 330
normalizaAngulo :: Angulo -> Angulo
normalizaAngulo a = normalizaAnguloAcc a a

normalizaAnguloAcc :: Angulo -> Angulo -> Angulo
normalizaAnguloAcc ang a | ang>= 360 = normalizaAnguloAcc (ang-360) a
                         | ang < 0 = normalizaAnguloAcc (ang+360) a
                         | otherwise = ang  

-- ** Funções sobre listas.

-- | Devolve o elemento num dado índice de uma lista.
--
-- __Sugestão:__ Não use a função (!!) :: [a] -> Int -> a :-)
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista n (x:xs) = if n==0 then x 
                                       else encontraIndiceLista (n-1) xs 

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista n a [] = []
atualizaIndiceLista n a (x:xs) = if n==0 then a:xs  
                                         else x:atualizaIndiceLista (n-1) a xs 

-- ** Funções sobre matrizes.

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatriz (a,b) ((h:hs):ks) = encontraPosicaoMatrizAcc (0,0) (a,b) ((h:hs):ks)

encontraPosicaoMatrizAcc :: (Int,Int) -> PosicaoMatriz -> Matriz a -> a
encontraPosicaoMatrizAcc (x,y) (a,b) ((h:hs):ks) | x/=a = encontraPosicaoMatrizAcc (x+1,y) (a,b) ks
                                                 | y/=b = encontraPosicaoMatrizAcc (x,y+1) (a,b) ((hs):ks)
                                                 | otherwise = h 

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz = undefined


