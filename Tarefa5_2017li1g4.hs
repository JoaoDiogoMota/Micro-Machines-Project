{-|
Module      : Tarefa5_2017li1g4
Description : Módulo da Tarefa 5 para LI1 17/18

Módulo para a realização da Tarefa 5 de LI1 em 2017/18.
-}
module Main where

import LI11718
import Graphics.Gloss
import Graphics.Gloss.Data.Picture          -- importar o tipo Picture
import Graphics.Gloss.Interface.Pure.Game   -- importar o tipo Event
import Tarefa4_2017li1g4
import Tarefa3_2017li1g4

{-|
Função principal usada para animar um jogo completo.
Compilar com o GHC.
-}
main :: IO ()
main = do inicio <- estadoInicial     
          play dm              -- display mode
               (greyN 0.7)     -- côr do fundo da janela
               fr              -- frame rate
               inicio          -- estado inicial
               desenhaEstado   -- desenha o estado do jogo
               reageEvento     -- reage a um evento
               reageTempo      -- reage ao passar do tempo

-- | A função __carroEX__ é um exemplo de um carro possível 
carroEX3 :: Carro
carroEX3 = Carro {posicao = (-1,1.5), direcao = 20, velocidade = (1,0)}

-- | A função __carroEX2__ é um segundo exemplo de um possível carro
carroEX4 :: Carro
carroEX4 = Carro {posicao = (-1,1.7), direcao = 30, velocidade = (1,0)}

-- | A função __t3__ é um exemplo de um 'Tabuleiro'
t3 :: Tabuleiro
t3 = [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca (Curva Norte) 1, Peca Recta 1, Peca Recta 1, Peca Recta 1, Peca (Curva Este) 1, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Recta 1,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Recta 1, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 0, Peca (Rampa Este) 0, Peca (Curva Sul) 1, Peca Lava 0, Peca (Curva Norte) 1, Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 0, Peca (Curva Este) 0, Peca Lava 0, Peca Lava 0, Peca Recta 1, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca (Rampa Norte) 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca Recta 0, Peca (Curva Sul) 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]

-- | A função __jogo3__ é um exemplo de um possível 'Jogo'

jogo3 :: Jogo
jogo3 = Jogo {mapa = Mapa ((2,1), Este) t3, pista = p, carros = [carroEXs,carroEX2], nitros = [], historico = [[(2,2)],[(2,2)]]}

-- | Representação do Estado do Jogo, em que este é constituído por um 'Tabuleiro', as respetivas imagens do essencial para a construção do mapa, um 'Float' correspondente ao tempo e dois ' Carro', a que corresponde o jogador e o bot.
type Estado = (Jogo,Tabuleiro, Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Float, Int, Int)

-- | Representação do Estado2 do Jogo, utilizado na função __auxdesenhaEstado__, constituído apenas por uma lista de 'Peca' e as imagens do jogo. 
type Estado2 = ([Peca],Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture)

-- | A função __estadoInicial__ é a função que representa o estado inicial do jogo, isto é, o seu início.
estadoInicial :: IO Estado
estadoInicial = do
        mcqueen <- loadBMP "images/mcqueen.bmp" 
        bot <- loadBMP "images/bot.bmp"
        floor <- loadBMP "images/floor.bmp"
        lava <- loadBMP "images/lava.bmp"
        sobeNorte <- loadBMP "images/sobeNorte.bmp"
        sobeSul <- loadBMP "images/sobeSul.bmp"
        sobeEste <- loadBMP "images/sobeEste.bmp"
        sobeOeste <- loadBMP "images/sobeOeste.bmp"
        curvaNorte <- loadBMP "images/curvaNorte.bmp"
        curvaSul <- loadBMP "images/curvaSul.bmp"
        curvaEste <- loadBMP "images/curvaEste.bmp"
        curvaOeste <- loadBMP "images/curvaOeste.bmp"
        return (Jogo{mapa = (Mapa((2,1),Este) t3),pista = p ,carros = [carroEX3, carroEX4],nitros = [], historico = []},t3, mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste, floor, lava, 60, 0, 0)

-- | A função __getsize__ recebe um Estado, calculando o número de colunas do mapa.
getsize :: Estado -> Float
getsize (jogo,(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2) = fromIntegral (length x)

-- | A função __moveCarro__ é responsável por permitir que o carro se mova aquando da pressão efetuada nas teclas, como indicado na função __reageEvento__, dando o Estado do jogo e movendo o carro uma coordenada.
moveCarro :: (Double,Double) -> Estado -> Estado
moveCarro (a,b) (Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1,carro2],nitros = n, historico = hist},(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2)
 = (Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [(movecar (a,b) carro1), carro2],nitros = n, historico = hist},(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2)

-- | A função __moveCarro2__ é responsável por permitir que o carro se mova aquando da pressão efetuada nas teclas, como indicado na função __reageEvento__, dando o Estado do jogo e movendo o carro uma coordenada.
moveCarro2 :: (Double,Double) -> Estado -> Estado
moveCarro2 (a,b) (Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1,carro2],nitros = n, historico = hist},(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2)
 = (Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1, (movecar (a,b) carro2)],nitros = n, historico = hist},(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2)

-- | A função __movecar__ calcula para que posição o carro irá após somar uma determinada coordenada (a,b).
movecar :: (Double,Double) -> Carro -> Carro
movecar (a,b) Carro{posicao = (c,l), direcao = d, velocidade = (v1,v2)} = Carro{posicao = (c + a,l + b), direcao = d, velocidade = (v1,v2)}

-- | Função que altera o estado do jogo quando acontece um evento.
-- Reage ao pressionar das setas do teclado, movendo a bola 0.25 pixéis numa direção.
reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) e = moveCarro (0,-0.25) e -- Player 1
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) e = moveCarro (0,0.25) e
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) e = moveCarro (-0.25,0) e
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) e = moveCarro (0.25,0) e
reageEvento (EventKey (Char 'w') Down _ _) e = moveCarro2 (0,-0.25) e -- Player 2
reageEvento (EventKey (Char 's') Down _ _) e = moveCarro2 (0,0.25) e
reageEvento (EventKey (Char 'a') Down _ _) e = moveCarro2 (-0.25,0) e
reageEvento (EventKey (Char 'd') Down _ _) e = moveCarro2 (0.25,0) e
reageEvento _ e = e -- ignora qualquer outro evento


-- | A função __reageTempo__ altera o estado do jogo quando o tempo avança @n@ segundos.
reageTempo :: Float -> Estado -> Estado
reageTempo n (j,x,a,b,g,m,q,c,d,f,w,e,y,z,t,s1,s2) | t-n <= 0 = (j,x,a,b,g,m,q,c,d,f,w,e,y,z,0,s1,s2)
                                                   | otherwise = (j,x,a,b,g,m,q,c,d,f,w,e,y,z,t-n,s1,s2)


--mapa em pictures, linha, coluna, pixelsize das imagens e tamanho do mapa 
-- | A função __desenhaMapa__ desenha o mapa em 'Picture', tendo por base o tamanho do mapa desejado, bem como o tamanho das imagens em pixéis, as linhas e as colunas
desenhaMapa :: [Picture] -> Float -> Float -> Float -> Float -> [Picture]
desenhaMapa [] _ _ _ _ = [] 
desenhaMapa (x:xs) l c ps k | c == (k - 1) = Translate (-((k*ps)/2) + (c*ps)) ((k*ps/2) - (l*ps)) x : desenhaMapa xs (l+1) 0 ps k 
                            | otherwise = Translate ((-((k*ps)/2)) + (c*ps)) ((k*ps/2) - (l*ps)) x : desenhaMapa xs l (c+1) ps k

-- | A função __desenhaEstado__ desenha a área do jogo, as bordas do mapa, os barcos que estarão em jogo e o tempo restante para o final do jogo.
desenhaEstado :: Estado -> Picture
desenhaEstado e@(Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = (h:hs),nitros = n, historico = hist},(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2) 
                | tempo > 0 = Scale 1 1 $ inGame 
                | otherwise = Translate (-(((getsize(e))*55)/2) + ((((getsize(e))+1)*10))) ((((getsize(e))*10/2) - 10)) (Scale 0.5 0.5 $ Text $ "WINNER!")
                where inGame = Pictures $ (desenhaMapa (auxdesenhaEstado e) 0 0 60 (getsize e)) ++ (desenhaCarros e) ++ [mostratempo e] ++ [mostrascore e] ++ [mostrascore2 e] 
                      mostratempo e = Translate (-(((getsize(e))*25)/2) + ((((getsize(e))+1)*40))) ((((getsize(e))*60/2) - 60)) (Scale 0.5 0.5 $ Text $ show $ round $ tempo) 
                      mostrascore e = Translate (-(((getsize(e))*25)/2) + ((((getsize(e))+1)*40))) ((((getsize(e))*35/2) - 60)) (Scale 0.5 0.5 $ Text $ show $  s1)
                      mostrascore2 e = Translate (-(((getsize(e))*25)/2) + ((((getsize(e))+1)*40))) ((((getsize(e))*10/2) - 60)) (Scale 0.5 0.5 $ Text $ show $ s2)

-- | A função __tempo__ indica-nos o 'Tempo', dado um 'Estado' do jogo. 
tempo :: Estado -> Float
tempo (jogo,(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,t,s1,s2) = t

-- | A função __auxdesenhaEstado__ é uma auxiliar da função __desenhaEstado__, cujo objetivo é o de indicar qual a imagem correspondente a todos os tipos de 'Peca' existentes no jogo. Para este efeito, recorremos a um novo 'Estado' (Estado2).
auxdesenhaEstado :: Estado -> [Picture]
auxdesenhaEstado (_,[],_,_,_,_,_,_,_,_,_,_,_,_,_,_,_) = [] 
auxdesenhaEstado (jogo,(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2) = picturematch (x,mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava) ++ auxdesenhaEstado (jogo,xs,mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2)
                                     where picturematch :: Estado2 -> [Picture]
                                           picturematch ([],_,_,_,_,_,_,_,_,_,_,_,_) = []
                                           picturematch ((x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava) 
                                                | tipo x == Lava = lava : picturematch (xs,mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste, floor,lava)
                                                | tipo x == Recta = floor : picturematch (xs,mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste, floor,lava) 
                                                | tipo x == Rampa (Norte) = sobeNorte : picturematch (xs,mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste, floor,lava) 
                                                | tipo x == Rampa (Sul) = sobeSul : picturematch (xs,mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste, floor,lava) 
                                                | tipo x == Rampa (Este) = sobeEste : picturematch (xs,mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste, floor,lava) 
                                                | tipo x == Rampa (Oeste) = sobeOeste : picturematch (xs,mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste, floor,lava)  
                                                | tipo x == Curva Norte = curvaNorte : picturematch (xs,mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava)  
                                                | tipo x == Curva Sul = curvaSul : picturematch (xs,mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste, floor,lava)
                                                | tipo x == Curva Este = curvaEste : picturematch (xs,mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste, floor,lava)
                                                | tipo x == Curva Oeste = curvaOeste : picturematch (xs,mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava)                                                                                   
                                                | otherwise = picturematch (xs,mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava) 

-- | A função __getCarros__ indica onde se encontra '[Carro]'
getCarros :: Estado -> [Carro]
getCarros (Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = cars,nitros = n, historico = hist},(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2) = cars

-- | A função __getMcqueen__ indica qual a imagem do carro 'mcqueen'
getMcqueen :: Estado -> Picture
getMcqueen (Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = cars,nitros = n, historico = hist},(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2) = mcqueen

-- | A função __getBot__ indica qual a imagem do carro 'bot'
getBot :: Estado -> Picture
getBot (Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = cars,nitros = n, historico = hist},(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2) = bot

-- | A função __desenhaCarros__ é responsável por desenhar ambos os carros
desenhaCarros :: Estado -> [Picture]
desenhaCarros e = aux (getCarros e) 0 (getMcqueen e) (getBot e)

-- | A função __aux__ é referente à função __desenhaCarros__ 
aux :: [Carro] -> Float -> Picture -> Picture -> [Picture]
aux [x] size _ bot = [desenhaCarro x size bot]
aux (h:t) size mcqueen bot = desenhaCarro h size mcqueen : aux t size mcqueen bot

-- | A função __desenhaBarco__ tem como objetivo desenhar o 'Carro' principal, para que este seja colocado no mapa.
desenhaCarro :: Carro -> Float -> Picture -> Picture 
desenhaCarro Carro{posicao = (c,l), direcao = d, velocidade = (v1,v2)} size img = Translate (60*(realToFrac c)) (-60*(realToFrac l)) img  

-- | A função __elemIndice__ é uma variação da função pré-definida /elemIndices/ em que, dado um duplo (Int,Int) e uma '[Posicao]', indica em que posições se encontra o duplo.
elemIndice :: (Int,Int) -> [Posicao] -> [Int]
elemIndice (x,y) pos = elemIndiceAux 0 (x,y) pos 

-- | A função __elemIndicesAux__ é uma auxiliar da função __elemIndice__ em que acrescenta um contador 'i', que vai percorrer a lista, guardando as posições em que se encontra o duplo.
elemIndiceAux :: Int -> (Int,Int) -> [Posicao] -> [Int]
elemIndiceAux i (a,b) [] = []
elemIndiceAux i (a,b) ((y,ys):pos) = if (a == y && b == ys) then i : elemIndiceAux (i+1) (a,b) pos 
                                    else elemIndiceAux (i+1) (a,b) pos

-- | A função __score1__ indica o número de vezes que o carro1 passa pela partida.
score1 :: Estado -> Int 
score1 (Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1,carro2],nitros = n, historico = [l1,l2]},(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2) = length (elemIndice (c,l) l1) - 1
                                                                                                                                                   
-- | A função __score2__ indica o número de vezes que o carro2 passa pela partida.
score2 :: Estado -> Int 
score2 (Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1,carro2],nitros = n, historico = [l1,l2]},(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2) = length (elemIndice (c,l) l2) - 1


-- | A função __s1__ indica qual o score do primeiro jogador.
s1 :: Estado -> Int 
s1 (Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1,carro2],nitros = n, historico = [l1,l2]},(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2) = s1 

-- | A função __s2__ indica qual o score do segundo jogador.
s2 :: Estado -> Int
s2 (Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1,carro2],nitros = n, historico = [l1,l2]},(x:xs),mcqueen,bot,sobeNorte,sobeSul,sobeEste,sobeOeste,curvaNorte,curvaSul,curvaEste,curvaOeste,floor,lava,tempo,s1,s2) = s2 

-- | Frame rate
fr :: Int
fr = 50 

-- | Display mode
dm :: Display
dm = InWindow "Micro Machines" (800, 600) (0,0)


{- | A função __atualizaMovimenta__ recebe um 'Tempo', 'Jogo' e uma lista de 'Acao', e é responsável por interligar as Tarefas 3 e 4 à Tarefa 5, de forma a que seja possível movimentar os carros e atualizá-los da forma pretendida. 

>>> atualizaMovimenta 1 jogo2 [acao,acao]
Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) 1,Peca Recta 1,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 2.0, k_pneus = 3.0, k_acel = 4.0, k_peso = 2.0, k_nitro = 1.0, k_roda = 180.0}, carros = [Carro {posicao = (1.0,1.0), direcao = 0.0, velocidade = (0.0,0.0)},Carro {posicao = (1.0,1.0), direcao = 0.0, velocidade = (0.0,0.0)}], nitros = [], historico = [[(1,1),(2,1),(2,1)],[(1,1),(2,1),(2,1)]]} 
-}

atualizaMovimenta :: Tempo -> Jogo -> [Acao] -> Jogo 
atualizaMovimenta tempo jogo a = novoJogo    
          where jogoAct = atualizaTodos tempo jogo a                
                novoJogo  = movimentaTodos tempo jogoAct


{- | A função __atualizaTodos__ recebe um 'Tempo', um 'Jogo' e uma lista de 'Acao', devolvendo o jogo após atualizar as ações dos jogadores

>>> atualizaTodos 1 jogo1 [acao,acao]
Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 2.0, k_pneus = 3.0, k_acel = 4.0, k_peso = 2.0, k_nitro = 1.0, k_roda = 180.0}, carros = [Carro {posicao = (1.0,1.0), direcao = 200.0, velocidade = (3.8923868329009514,3.0065601119742285)},Carro {posicao = (1.0,2.0), direcao = 210.0, velocidade = (3.6790132368051602,1.0726285476869593)}], nitros = [], historico = [[(1,1),(2,1),(2,1)],[(1,2),(2,1),(2,1)]]}
-}

atualizaTodos :: Tempo -> Jogo -> [Acao] -> Jogo
atualizaTodos tempo jogo@Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1,carro2],nitros = n, historico = his} [] = jogo
atualizaTodos tempo jogo (x:xs) = auxAtualiza tempo jogo 0 (x:xs) 


-- | A função __auxAtualiza__ é uma auxiliar da função __atualizaTodos__, responsável por atualizar um jogador e aplicar a recursividade, atualizando os restantes jogadores.
auxAtualiza :: Tempo -> Jogo -> Int -> [Acao] -> Jogo
auxAtualiza tempo jogo@Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1,carro2],nitros = n, historico = his} i [] = jogo 
auxAtualiza tempo jogo i (x:xs) = if (i <= 2 ) then auxAtualiza tempo (atualiza tempo jogo i x) (i+1) xs  
                                  else jogo

{- | A função __justToCarro__ recebe um 'Maybe Carro' e devolve um 'Carro'

>>> justToCarro (Just Carro{posicao = (2,1), direcao = 20, velocidade = (1,0)})
Carro {posicao = (2.0,1.0), direcao = 20.0, velocidade = (1.0,0.0)}
-}

justToCarro :: Maybe Carro -> Carro 
justToCarro (Just a) = a 
justToCarro Nothing = error "error"


{- A função __movimentaTodos__ recebe um 'Tempo', um 'Jogo' e um 'Int', devolvendo o novo 'Jogo' após atualizar o movimento de todos os jogadores.

>>> movimentaTodos 1 jogo2
Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 2.0, k_pneus = 3.0, k_acel = 4.0, k_peso = 2.0, k_nitro = 1.0, k_roda = 180.0}, carros = [Carro {posicao = (1.0,1.0), direcao = 0.0, velocidade = (1.0,0.0)},Carro {posicao = (1.0,1.0), direcao = 0.0, velocidade = (1.0,0.0)}], nitros = [], historico = [[(2,1)],[(2,1)]]}

-} 
movimentaTodos :: Tempo -> Jogo -> Jogo 
movimentaTodos tempo jogo@Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1,carro2],nitros = n, historico = his} = auxMovimenta tempo jogo 0 


-- | A função __auxMovimenta__ é uma auxiliar da função __movimentaTodos__, responsável por efetuar o movimento de um jogador e aplicar a recursividade aos restantes jogadores.
auxMovimenta :: Tempo -> Jogo -> Int -> Jogo 
auxMovimenta tempo jogo@Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = car,nitros = n, historico = his} i 
                         | (i < 2) = auxMovimenta tempo (afterDestroi jogo i tempo (movimenta (tab jogo) tempo (car !! i))) (i+1)  
                         | otherwise = jogo     


-- | A funçãp __posAntes__ recebe um 'Jogo' e um 'Int', indicando qual é a última posição de determinado jogador
posAntes :: Jogo -> Int -> Posicao 
posAntes jogo@Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1,carro2],nitros = n, historico = his} pos = head (his !! pos)


-- | A função __retorna__ recebe um 'Carro' e uma 'Posicao', colocando o 'Carro' nessa 'Posicao' a velocidade (0,0) e direção 0.
retorna :: Carro -> Posicao -> Carro 
retorna Carro{posicao = (c,l), direcao = x, velocidade = (v1,v2)} (c1,l1) = Carro{posicao = (fromIntegral c1, fromIntegral l1), direcao = 0, velocidade = (0,0)} 


{- A função __afterDeath__ recebe um 'Carro', um 'Jogo', um 'Tempo' e um 'Int' pelo que, caso um carro seja destruído, regressa à posição onde estava imediatamente antes de morrer.

>>> afterDestroi jogo2 0 1 (Just carroEX)
Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 2.0, k_pneus = 3.0, k_acel = 4.0, k_peso = 2.0, k_nitro = 1.0, k_roda = 180.0}, carros = [Carro {posicao = (2.5,1.5), direcao = 0.0, velocidade = (1.0,0.0)},Carro {posicao = (1.5,1.5), direcao = 0.0, velocidade = (1.0,0.0)}], nitros = [], historico = [[(2,1)],[(2,1)]]}
-}

afterDestroi :: Jogo -> Int -> Tempo -> Maybe Carro -> Jogo
afterDestroi jogo@Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = car,nitros = n, historico = his} x tempo Nothing = Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = elemento car x (retorna (car !! x) (posAntes jogo x)),nitros = n, historico = his}
afterDestroi jogo@Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1, carro2],nitros = n, historico = his} x tempo carro | (justToCarro carro) == carro1 = Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [justToCarro (movimenta t tempo carro1), carro2],nitros = n, historico = his}
                                                                                                                                  | (justToCarro carro) == carro2 = Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1, justToCarro(movimenta t tempo carro2)],nitros = n, historico = his}


-- | A função __tab__ recebe um 'Jogo' e indica qual o 'Tabuleiro' desse jogo.
tab :: Jogo -> Tabuleiro 
tab Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = [carro1,carro2],nitros = n, historico = his} = t