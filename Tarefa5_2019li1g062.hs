-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

import LI11920
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy

type EstadoGloss = (Estado, [Picture])  


-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
main :: IO ()
main = do 
    Just recta <- loadJuicy "C://Users/joana/OneDrive/Ambiente de Trabalho/Uni/LI1/ImagensProjeto/RectaRelva.png" 
    Just fundo <- loadJuicy "C://Users/joana/OneDrive/Ambiente de Trabalho/Uni/LI1/ImagensProjeto/FundoTeste.jpg"
    play dm         -- janela onde irá correr o jogo
         (greyN 0.5)     -- côr do fundo da janela
         fr              -- frame rate
         (estadoGlossInicial [(Scale 0.1 0.1 recta), fundo])  -- estado inicial
         desenhaEstadoGloss   -- desenha o estado do jogo
         reageEventoGloss     -- reage a um evento
         reageTempoGloss      -- reage ao passar do tempo
--          p <- loadBMP "imagem.bmp"
--          Just img1 <- loadJuicy “imagem.jpg”
--          Just img2 <- loadJuicy “imagem.png”

--- Estado Jogo 
estadoInicial :: Estado
estadoInicial = (Estado (stringParaMapa mapa) [(Jogador 0 0 0 0 (Chao True))])

desenhaEstado :: Estado -> [Picture] -> [Picture] 
desenhaEstado (Estado m ((Jogador _ d _ _ _):_)) l = desenhaMapa m 0 0 l 

reageEvento :: Event -> Estado -> Estado
reageEvento _ s = s

reageTempo :: Float -> Estado -> Estado
reageTempo n e = e 

{--desenhaEstado :: Estado -> [Picture] -> [Picture]
desenhaEstado (Estado m js) p = desenhaMapa m width height
--}
--- Estado Gloss

estadoGlossInicial :: [Picture] -> EstadoGloss
estadoGlossInicial l = (estadoInicial, l) 

desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (e,l) = estado 
                      where estado = Pictures (desenhaEstado e l)
{--
desenhaEstadoGloss (x,y) = Pictures [fundo,estado] 
             where estado = Pictures (desenhaEstado e estadoImagens)
                   estadoImagens = listaEntre2Indices 1 _ lista 
                   fundo = head lista 
--}
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
{--
reageEventoGloss (EventKey (SpecialKey KeyUp)    Down _ _) (x,y) = (x,y+5)
reageEventoGloss (EventKey (SpecialKey KeyDown)  Down _ _) (x,y) = (x,y-5)
reageEventoGloss (EventKey (SpecialKey KeyLeft)  Down _ _) (x,y) = (x-5,y)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (x,y) = (x+5,y)
--}
reageEventoGloss _ s = s -- ignora qualquer outro evento

reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss n (e,l) = (e,l)

fr :: Int
fr = 150

dm :: Display
dm = FullScreen
--dm = InWindow "Novo Jogo" (400, 400) (0, 0)

desenhaPista :: Pista -> Float -> Float -> [Picture] -> [Picture]
desenhaPista [] _ _ _ = []
desenhaPista (h:t) x y p = (desenhaPeca h x y p) : (desenhaPista t (x+1) y p)

desenhaPeca :: Peca -> Float -> Float -> [Picture] -> Picture
desenhaPeca p x y (pi:pis) = Translate x y pi  

desenhaMapa :: Mapa -> Float -> Float -> [Picture] -> [Picture]
desenhaMapa [] _ _ _ = []
desenhaMapa (h:t) x y p = (desenhaPista h x y p) ++ (desenhaMapa t x (y+1) p)

mapa :: [String]
mapa = ["_ _"]

stringParaMapa :: [String] -> Mapa 
stringParaMapa (h:t) = (stringParaPista 0 h) : (stringParaMapa t)
stringParaMapa _ = []

stringParaPista :: Int -> String -> Pista 
stringParaPista c (h:t) = (charToBloco h) : (stringParaPista (c+1) t)
stringParaPista _ _ = []

charToBloco :: Char -> Peca 
charToBloco x | x== '_' = Recta Relva _  










