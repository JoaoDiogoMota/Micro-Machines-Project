{-|
Module      : Tarefa 3
Description : Terceira tarefa do projeto de LI1 2017/2018

Funções que permitem o movimento do carro
-} 

module Tarefa3_2017li1g4 where

import LI11718

-- | A função __carroEX__ é um exemplo de um carro possível 
carroEX :: Carro
carroEX = Carro {posicao = (1, 1), direcao = 0, velocidade = (1,0)}

-- | A função __t__ serve como exemplo de um 'Tabuleiro'
t :: Tabuleiro
t = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- | A função __testesT3__ recebe listas com triplos de 'Tabuleiro', período de 'Tempo' e 'Carro', verificando o funcionamento da Tarefa 3, isto é, confirma que o novo estado do carro após se ter movimentado é válido
testesT3 :: [(Tabuleiro,Tempo,Carro)]
testesT3 = [([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], 1, Carro {posicao = (1.5, 1.5), direcao = 0, velocidade = (1,0)}),([[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 0, Peca (Rampa Oeste) 1, Peca Recta 1,Peca (Curva Este) 1, Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0, Peca Lava 0,Peca Recta 1,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0,Peca (Rampa Este) 0, Peca Recta 1, Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0, Peca Lava 0]],1,Carro{posicao=(1,2), direcao=0, velocidade= (0,1)})]

{- | A função __movimenta__ recebe um 'Tabuleiro', um período de 'Tempo' e um 'Carro', calculando o novo estado do carro após se movimentar

>>> movimenta t 1 carroEX 
Just (Carro {posicao = (2.5,1.5), direcao = 0.0, velocidade = (1.0,0.0)})
-}
movimenta :: Tabuleiro -> Tempo -> Carro -> Maybe Carro
movimenta m t carro@(Carro {posicao = (c,l), direcao = angulo, velocidade = (v1,v2)}) 
    | actualPos == Peca Lava 0 = Nothing 
    | (altAtual actualPos) - (altAtual newPos) > 0 = rampaAntes t (actualPos) (newPos) carro 
    | (altAtual actualPos) - (altAtual newPos) < 0 = Just (Carro {posicao = newPonto, direcao = 180 - angulo, velocidade = (v1,v2)})
    | otherwise = Just (Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)})
    where newPonto = nextPosition carro t
          newPos = wherePosition m (nextPosition carro t)
          actualPos = wherePosition m newPonto

-- | A função _rampaAntes__ calcula qual será o novo estado do carro na presença de rampas
rampaAntes :: Tempo -> Peca -> Peca -> Carro -> Maybe Carro 
rampaAntes t (Peca tipo altura) (Peca tipo2 altura2) carro@(Carro {posicao = (c,l), direcao = angulo, velocidade = (v1,v2)})
    | tipo == tipo2 && (altura + 1) == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)} -- duas subidas
    | tipo == tipo2 && altura == (altura2 + 1) = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)} -- duas descidas
    | tipo == tipo2 && altura == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo == Rampa Norte && altura == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo == Rampa Norte && altura + 1 == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo == Rampa Sul && altura == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo == Rampa Sul && altura + 1 == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo == Rampa Este && altura == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo == Rampa Este && altura + 1 == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo == Rampa Oeste && altura == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo == Rampa Oeste && altura + 1 == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo2 == Rampa Norte && altura + 1 == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo2 == Rampa Norte && altura == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo2 == Rampa Sul && altura + 1 == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo2 == Rampa Sul && altura == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo2 == Rampa Este && altura == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo2 == Rampa Este && altura + 1 == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo2 == Rampa Oeste && altura == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | tipo2 == Rampa Oeste && altura + 1 == altura2 = Just Carro {posicao = newPonto, direcao = angulo, velocidade = (v1,v2)}
    | otherwise = Nothing
    where newPonto = nextPosition carro t
                                

-- | A função __altAtual__ vai devolver a altura dando uma Peca
altAtual :: Peca -> Int
altAtual (Peca x y) = y  

{- | A função __nextPosition__ recebe um 'Carro' e um período de 'Tempo', calculando qual irá ser a posição do carro após o seu movimento, considerando a sua posição inicial, direção e velocidade

>>> nextPosition (Carro {posicao = (1.5, 1.5), direcao = 0, velocidade = (1,0)}) 1
(2.5,1.5)
-}
nextPosition :: Carro -> Tempo -> Ponto  
nextPosition (Carro {posicao = (c,l), direcao = angulo, velocidade = (v1,v2)}) t = ((c + t * v1) + 0.5 , (l + t * v2) + 0.5)


{- | A função __wherePosition__ recebe um 'Tabuleiro' e um 'Ponto', indicando qual a peça que se encontra nessa posição convertida a duplo de inteiros

>>> wherePosition t (1.5,1.5)
Peca (Curva Norte) 0
-}
wherePosition :: Tabuleiro -> Ponto -> Peca
wherePosition t (c,l) = wherePositionAUX t (c - 0.5, l - 0.5)

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