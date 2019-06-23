{-|
Module      : Tarefa6_2017li1g4
Description : Módulo da Tarefa 6 para LI1 17/18

Módulo para a realização da Tarefa 6 de LI1 em 2017/18.
-}
module Tarefa6_2017li1g4 where

import LI11718

-- | A função __a__ é uma Ação de exemplo
a :: Acao 
a = Acao {acelerar = True, travar = False, esquerda = False, direita = False, nitro = Nothing}

-- | A função __m__ é um Mapa de exemplo
m :: Mapa 
m = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- | A função __t__ é um Tabuleiro de exemplo
t :: Tabuleiro 
t = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca Recta 0,Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- | A função __p__ representa um exemplo de propriedades sensatas para um percurso dito normal
p :: Propriedades
p = Propriedades 2 3 4 2 15 180 

-- | A função __jogo1__ é um exemplo de um possível 'Jogo'
jogo1 :: Jogo
jogo1 = Jogo {mapa = Mapa ((2,1), Este) t, pista = p, carros = [carroEXs], nitros = [], historico = [[(2,1),(2,2),(2,3),(2,4)],[(3,1),(3,2),(3,3)]] }

-- | A função __carroEX__ é um exemplo de um carro possível 
carroEXs :: Carro
carroEXs = Carro {posicao = (3,1), direcao = 20, velocidade = (1,0)}

{- | Função usada para simular um /bot/ no jogo /Micro Machines/. Em cada instante, dado o tempo decorrido, o estado do jogo e o identificador do jogador, toma uma ação.

>>> bot 3 jogo1 1 
Acao {acelerar = True, travar = False, esquerda = False, direita = False, nitro = Nothing}
-}

bot :: Tempo -> Jogo -> Int -> Acao 
bot tempo jogo@(Jogo {mapa = (Mapa((c,l),ori) t), pista = p, carros = (x:xs), historico = h}) n = if tempo /= 0 then moveBot t ori x 
                                                                                                  else Acao {acelerar = False, travar = False, esquerda = False, direita = False, nitro = Nothing}


{- | A função __ondePos__ recebe um 'Carro' e um 'Tabuleiro' e, de acordo com a posição onde se encontra, devolve a Peca onde ele está.

>>> ondePos carroEXs t 
Peca Recta 0
-}
ondePos :: Carro -> Tabuleiro -> Peca 
ondePos Carro{posicao = (c,l), direcao = x, velocidade = (v1,v2)} t = wherePositionAUX t (c,l)  


-- | A função __wherePositionAUX__ recebe um 'Tabuleiro' e um 'Ponto', indicando qual a peça correspondente a essa posição
wherePositionAUX :: Tabuleiro -> Ponto -> Peca 
wherePositionAUX [] _ = (Peca Lava 0)
wherePositionAUX tab p  = (aux3 tab p 0 0)


-- | A função __aux3__ procura se uma peça se encontra em determinada linha, guardando o seu valor em /y/. Caso encontre recorre a __aux4__ a fim de descobrir qual a coluna em que essa peça se encontra
aux3 :: Tabuleiro -> Ponto -> Double -> Double -> Peca 
aux3 [] (c,l) x y = Peca Lava 0 
aux3 (z:zs) (c,l) x y = if y == l then aux4 z c x 
                        else aux3 zs (c,l) x (y+1)


-- | A função __aux4__ procura se uma peça se encontra em determinada coluna, guardando o seu valor em /c/ após saber em que linha se encontra.
aux4 :: [Peca] -> Double -> Double -> Peca
aux4 [] n c = Peca Lava 0
aux4 (x:xs) n c = if n == c then x  
                  else aux4 xs n (c+1)     


-- | A função __tipo__ recebe uma 'Peca' e devolve o tipo da mesma.
tipo :: Peca -> Tipo
tipo (Peca x y) = x


{- | A função __moveBot__ recebe um 'Tabuleiro', uma 'Orientacao' e um 'Carro' pelo que vai indicar qual o movimento que o carro deverá fazer dependendo da peça onde se encontra, dando a sua Ação.

>>> moveBot t Este Carro{posicao = (4,1), direcao = 0, velocidade = (0,1)}
Acao {acelerar = False, travar = True, esquerda = False, direita = True, nitro = Nothing}
-}
moveBot :: Tabuleiro -> Orientacao -> Carro -> Acao
moveBot t ori Carro{posicao = (c,l), direcao = x, velocidade = (v1,v2)} | tipo(wherePositionAUX t (c,l)) ==  Recta && (ori == Norte || ori == Sul || ori == Este || ori == Oeste) = Acao {acelerar = True, travar = False, esquerda = False, direita = False, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Curva Este) && ori == Norte = Acao {acelerar = False, travar = True, esquerda = True, direita = False, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Curva Oeste) && ori == Sul = Acao {acelerar = False, travar = True, esquerda = True, direita = False, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Curva Sul) && ori== Este = Acao {acelerar = False, travar = True, esquerda = True, direita = False, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Curva Norte) && ori == Oeste = Acao {acelerar = False, travar = True, esquerda = True, direita = False, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Curva Norte) && ori == Norte = Acao {acelerar = False, travar = True, esquerda = False, direita = True, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Curva Sul) && ori == Sul = Acao {acelerar = False, travar = True, esquerda = False, direita = True, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Curva Este) && ori == Este = Acao {acelerar = False, travar = True, esquerda = False, direita = True, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Curva Oeste) && ori == Oeste = Acao {acelerar = False, travar = True, esquerda = False, direita = True, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Curva Norte) && (ori == Sul || ori == Este) = Acao {acelerar = True, travar = False, esquerda = False, direita = False, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Curva Sul) && (ori == Norte || ori == Oeste) = Acao {acelerar = True, travar = False, esquerda = False, direita = False, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Curva Este) && (ori == Oeste || ori == Sul) = Acao {acelerar = True, travar = False, esquerda = False, direita = False, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Curva Oeste) && (ori == Este || ori == Norte) = Acao {acelerar = True, travar = False, esquerda = False, direita = False, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Rampa Este) && (ori == Este || ori == Oeste) = Acao {acelerar = True, travar = False, esquerda = False, direita = False, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Rampa Oeste) && (ori == Oeste || ori == Este) = Acao {acelerar = True, travar = False, esquerda = False, direita = False, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Rampa Norte) && (ori == Norte || ori == Sul) = Acao {acelerar = True, travar = False, esquerda = False, direita = False, nitro = Nothing}
                                                                        | tipo(wherePositionAUX t (c,l)) ==  (Rampa Sul) && (ori == Sul || ori == Norte) = Acao {acelerar = True, travar = False, esquerda = False, direita = False, nitro = Nothing}
                                                                        | otherwise = Acao {acelerar = False, travar = False, esquerda = False, direita = False, nitro = Nothing}















