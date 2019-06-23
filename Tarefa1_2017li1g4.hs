{-|
Module      : Tarefa 1
Description : Primeira tarefa do trabalho de LI1 de 2017/2018

Funções que concretizam a criação de um mapa através de um caminho dado 
-}

module Tarefa1_2017li1g4 where

import LI11718

-- | A função __testesT1__ recebe um caminho, verificando o funcionamento da Tarefa 1 
testesT1 :: [Caminho]
testesT1 = [[Avanca, CurvaDir, Sobe, CurvaDir, Avanca, CurvaDir, Desce, CurvaDir],[Avanca,Avanca,Avanca,Avanca,CurvaDir,CurvaDir,Avanca,CurvaEsq,Avanca,CurvaDir,Avanca,Avanca,Avanca,CurvaDir,Avanca,Avanca,CurvaDir,Avanca],[Avanca, CurvaDir, Avanca, Avanca, CurvaDir, CurvaDir, Avanca, CurvaEsq, CurvaEsq, Avanca, CurvaDir, CurvaDir, Avanca, Avanca, CurvaDir, Avanca],[Avanca, Sobe, Avanca, CurvaEsq, Avanca, CurvaDir, Avanca, Desce, CurvaEsq, Avanca, CurvaEsq, Avanca, Avanca, Avanca, Avanca, Avanca, Avanca, CurvaEsq, Avanca, Avanca, Avanca, CurvaEsq],[Avanca, Avanca, Avanca, Avanca, Avanca, CurvaDir, Avanca, CurvaEsq, Avanca, Avanca, CurvaEsq, Sobe, CurvaEsq, Avanca, CurvaDir, Desce, CurvaEsq, Avanca, Avanca, Avanca, Avanca, Avanca, Avanca, Avanca, CurvaEsq, Avanca, CurvaEsq, Avanca], [Avanca, Avanca, Avanca, CurvaDir, Avanca, CurvaDir, Avanca, Sobe, Avanca, Desce, CurvaDir, Avanca, CurvaDir, Avanca], [Avanca, CurvaDir, CurvaEsq, Sobe, CurvaDir, Avanca, CurvaDir, Avanca, Desce, Avanca, Avanca, CurvaDir, Avanca, Avanca, CurvaDir, Avanca], [], [Avanca, CurvaDir, Avanca, Sobe, Avanca, Desce, Avanca, CurvaDir, Avanca, CurvaDir, Avanca, Avanca, Avanca, Avanca, Avanca, CurvaDir], [Avanca, CurvaEsq, Sobe, CurvaDir, Avanca, Desce, CurvaDir, Avanca, Avanca, CurvaDir, Avanca, Avanca, Avanca, Avanca, Avanca, CurvaDir, CurvaDir, Avanca], [Avanca, CurvaDir, Avanca, Avanca, Avanca, CurvaDir, Avanca, Avanca, Avanca, CurvaDir, Avanca, Sobe, Avanca, Desce, Avanca, CurvaDir, Avanca, CurvaDir, Avanca, CurvaEsq],[Avanca,CurvaDir,Avanca,Sobe,Avanca,Avanca,Desce,Avanca,CurvaDir,Avanca,Avanca,Avanca,CurvaDir,CurvaDir,CurvaEsq,Avanca,Avanca,Avanca,Avanca,Avanca,CurvaDir,Avanca]]


-- | A função __constroi__ recebe um Caminho e cria o Mapa
constroi :: Caminho -> Mapa
constroi caminho = 
     Mapa (partida caminho, dirInit) (replace (tabuleiroLava (dimensao caminho)) (cria caminho (partida caminho) dirInit altInit))

-- | A função __cria__  recebe um Caminho, uma Posição, uma Orientação e uma Altura, e indica qual será a posição seguinte consoante o tipo de PASSO, associando uma função auxiliar a cada tipo de Passo 
cria :: Caminho -> Posicao -> Orientacao -> Altura -> [(Peca, Posicao)]
cria [] (c,l) direcao altura = []
cria (x:xs) (c,l) direcao altura  | x == Avanca = criaAvanca (x:xs) (c,l) direcao altura
                                  | x == Sobe = criaSobe (x:xs) (c,l) direcao altura  
                                  | x == Desce = criaDesce (x:xs) (c,l) direcao altura 
                                  | x == CurvaDir = criaDir (x:xs) (c,l) direcao altura 
                                  | x == CurvaEsq = criaEsq (x:xs) (c,l) direcao altura 

-- | A função __criaDir__ ,auxiliar da função __cria__ , recebe um Caminho, uma Posição, uma Orientação e uma Altura e, sabendo que se trata de uma curva à direita, irá devolver qual a posição após a curva
criaDir :: Caminho -> Posicao -> Orientacao -> Altura -> [(Peca, Posicao)]
criaDir [] (c,l) direcao altura = []
criaDir (x:xs) (c,l) direcao altura | x == CurvaDir && direcao == Este = (Peca (Curva Este) altura, (c,l)) : cria xs (c,l+1) Sul altura 
                                    | x == CurvaDir && direcao == Oeste = (Peca (Curva Oeste) altura, (c,l)) : cria xs (c,l-1) Norte altura 
                                    | x == CurvaDir && direcao == Norte = (Peca (Curva Norte) altura, (c,l)) : cria xs (c+1,l) Este altura 
                                    | x == CurvaDir && direcao == Sul = (Peca (Curva Sul) altura, (c,l)) : cria xs (c-1,l) Oeste altura 
 
-- | A função __criaEsq__ ,auxiliar da função __cria__ , recebe um Caminho, uma Posição, uma Orientação e uma Altura e, sabendo que se trata de uma curva à esquerda, irá devolver qual a posição após a curva
criaEsq :: Caminho -> Posicao -> Orientacao -> Altura -> [(Peca, Posicao)]
criaEsq [] (c,l) direcao altura = []
criaEsq (x:xs) (c,l) direcao altura | x == CurvaEsq && direcao == Este = (Peca (Curva Sul) altura, (c,l)) : cria xs (c,l-1) Norte altura 
                                    | x == CurvaEsq && direcao == Oeste = (Peca (Curva Norte) altura, (c,l)) : cria xs (c,l+1) Sul altura 
                                    | x == CurvaEsq && direcao == Norte = (Peca (Curva Este) altura, (c,l)) : cria xs (c-1,l) Oeste altura 
                                    | x == CurvaEsq && direcao == Sul = (Peca (Curva Oeste) altura, (c,l)) : cria xs (c+1,l) Este altura 

-- | A função __criaSobe__ ,auxiliar da função __cria__ , recebe um Caminho, uma Posição, uma Orientação e uma Altura e, sabendo que se trata de uma subida, irá devolver qual a posição após a mesma
criaSobe :: Caminho -> Posicao -> Orientacao -> Altura -> [(Peca, Posicao)]
criaSobe [] (c,l) direcao altura = []
criaSobe (x:xs) (c,l) direcao altura | x == Sobe && direcao == Este = (Peca (Rampa Este) altura, (c,l)) : cria xs (c+1,l) direcao (altura+1) 
                                     | x == Sobe && direcao == Oeste = (Peca (Rampa Oeste) altura, (c,l)) : cria xs (c-1,l) direcao (altura+1) 
                                     | x == Sobe && direcao == Norte = (Peca (Rampa Norte) altura, (c, l)) : cria xs (c,l-1) direcao (altura+1)  
                                     | x == Sobe && direcao == Sul = (Peca (Rampa Sul) altura, (c,l)) : cria xs (c,l+1) direcao (altura+1)  

-- | A função __criaDesce__ ,auxiliar da função __cria__ , recebe um Caminho, uma Posição, uma Orientação e uma Altura e, sabendo que se trata de uma descida, irá devolver qual a posição após a mesma
criaDesce :: Caminho -> Posicao -> Orientacao -> Altura -> [(Peca, Posicao)]
criaDesce [] (c,l) direcao altura = []
criaDesce (x:xs) (c,l) direcao altura | x == Desce && direcao == Este = (Peca (Rampa Oeste) (altura-1), (c,l)) : cria xs (c+1,l) direcao (altura-1) 
                                      | x == Desce && direcao == Oeste = (Peca (Rampa Este) (altura-1), (c,l)) : cria xs (c-1,l) direcao (altura-1) 
                                      | x == Desce && direcao == Norte = (Peca (Rampa Sul) (altura-1), (c,l)) : cria xs (c,l-1) direcao (altura-1)  
                                      | x == Desce && direcao == Sul = (Peca (Rampa Norte) (altura-1), (c,l)) : cria xs (c,l+1) direcao (altura-1) 

-- | A função __criaAvanca__ ,auxiliar da função __cria__ , recebe um caminho, uma Posição, uma Orientação e uma Altura e, sabendo que se trata de uma peça Avanca, irá devolver qual a posição após a mesma
criaAvanca :: Caminho -> Posicao -> Orientacao -> Altura -> [(Peca, Posicao)]
criaAvanca [] (c,l) direcao altura = []
criaAvanca (x:xs) (c,l) direcao altura | x == Avanca && direcao == Este = (Peca Recta altura, (c,l)) : cria xs (c+1,l) direcao altura 
                                       | x == Avanca && direcao == Oeste = (Peca Recta altura, (c,l)) : cria xs (c-1,l) direcao altura 
                                       | x == Avanca && direcao == Norte = (Peca Recta altura, (c,l)) : cria xs (c,l-1) direcao altura 
                                       | x == Avanca && direcao == Sul = (Peca Recta altura, (c,l)) : cria xs (c,l+1) direcao altura 

-- | Esta função, __tabuleiroLava__ constrói um tabuleiro em que as peças são todas do tipo "Lava"
tabuleiroLava :: Dimensao -> [[Peca]]
tabuleiroLava (x,y) = replicate y (replicate x (Peca Lava 0)) 


-- | A função __replace__ vai substituir o tabuleiro criado na função __tabuleiroLava__ de apenas peças do tipo "Lava" pelas peças que vão constituir o caminho 
replace :: Tabuleiro -> [(Peca, Posicao)] -> Tabuleiro 
replace t [] = t
replace t ((peca, (c,l)):xs) = replace (take l t ++ [take c (t !! l) ++ [peca] ++ drop (c+1) (t !! l)] ++ drop (l+1) t) xs