{-|
Module      : Tarefa 2
Description : Segunda tarefa do projeto de LI1 2017/2018

Funções que validam os mapas construídos na Tarefa 1
-} 

module Tarefa2_2017li1g4 where

import LI11718

-- | Linhas num 'Tabuleiro'
type Linha = Int

-- | Colunas num 'Tabuleiro'
type Coluna = Int

-- | A função __t__ serve como exemplo de um 'Tabuleiro'
t :: Tabuleiro 
t = [[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca (Curva Norte) 1, Peca Recta 1, Peca Recta 1, Peca Recta 1, Peca (Curva Este) 1, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Recta 1,Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Recta 1, Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 0, Peca (Rampa Este) 0, Peca (Curva Sul) 1, Peca Lava 0, Peca (Curva Norte) 1, Peca Recta 1,Peca (Curva Sul) 1, Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 0, Peca (Curva Este) 0, Peca Lava 0, Peca Lava 0, Peca Recta 1, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Recta 0, Peca Lava 0, Peca Lava 0, Peca (Rampa Norte) 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0, Peca Recta 0, Peca (Curva Sul) 0, Peca Lava 0, Peca Lava 0, Peca Lava 0],[Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0, Peca Lava 0]]


-- | A função __testesT2__ recebe uma lista de tabuleiros, verificando o funcionamento da Tarefa 2 
testesT2 :: [Tabuleiro]
testesT2 = [[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 1,Peca Recta 1, Peca Recta 1, Peca (Curva Este) 1, Peca Lava 0],[Peca Lava 0,Peca Recta 1,  Peca Lava 0,Peca Lava 0,Peca (Rampa Norte) 0, Peca Lava 0],[Peca Lava 0,Peca (Rampa Norte) 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca (Curva Norte) 0, Peca Recta 0,Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 0, Peca (Curva Sul) 0, Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0, Peca (Curva Norte) 0,Peca (Rampa Este) 0, Peca Recta 1,Peca (Rampa Oeste) 0, Peca Recta 0, Peca (Curva Este) 0, Peca Lava 0],[Peca Lava 0,Peca Recta 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Recta 0,Peca Lava 0],[Peca Lava 0, Peca (Curva Oeste) 0, Peca Recta 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0, Peca Recta 0, Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0, Peca (Curva Oeste) 0,Peca Recta 0, Peca (Curva Sul) 0,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],[[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]]


-- | A função __valida__ tem como propósito verificar que um mapa é totalmente passível de ser jogado
valida :: Mapa -> Bool
valida (Mapa ((c,l),x) t) = validaLava t && validaExtremos t 0 && validaRetangular (Mapa ((c,l),x) t) && validaNotCaminho t (retiraCaminho t (c,l) x []) && caminhoNLava (retiraCaminho t (c,l) x []) && validaProximo (retiraCaminho t (c,l) x [])  && validaAltura (Mapa ((c,l),x) t)


-- | A função __validaLavaL__ verifica linha a linha se as peças do tipo "Lava" têm altura 0
validaLavaL :: [Peca] -> Bool 
validaLavaL [] = True
validaLavaL (Peca Lava altura:xs) = (altura == 0) && validaLavaL xs 
validaLavaL (_:xs) = validaLavaL xs 


-- | A função __validaLava__ vai certificar-se de que todas as peças do tipo "Lava" se encontram a uma altura 0, recorrendo à função auxiliar __validaLavaL__
validaLava :: Tabuleiro -> Bool 
validaLava [] = True 
validaLava xs = foldr ((&&) . validaLavaL) True xs


-- | A função __validaExtremos__ assegura que as primeira e última linhas e colunas do tabuleiro são constituídas unicamente por (Peca Lava 0)
validaExtremos :: Tabuleiro -> Int -> Bool
validaExtremos [] n = True
validaExtremos (x:xs) n | head x == Peca Lava 0 && last x == Peca Lava 0 = validaExtremos xs (n + 1)
                        | otherwise = False    


-- | A função __validaRetangular__ tem como objetivo verificar se o mapa toma sempre a forma retangular, recorrendo a uma função auxiliar __validaRetangularAux__
validaRetangular :: Mapa -> Bool 
validaRetangular (Mapa (posicao, orientacao) (x:xs)) = validaRetangularAux (length x) xs 


-- | A função __validaRetangularAux__ verifica se todas as linhas do tabuleiro têm o mesmo comprimento
validaRetangularAux :: Int -> [[a]] -> Bool
validaRetangularAux _ [] = True 
validaRetangularAux n (x:xs) = n == length x && validaRetangularAux n xs
                                                   
                    
-- caso lava valida orientacoes
-- como o percurso completa uma volta, assume-se que as orientações estejam corretas 
{- | A função __retiraCaminho__ recebe um Tabuleiro, uma Posicao, uma Orientacao, e uma lista de triplos (Peca,Posicao,Orientacao) e devolve uma lista de triplos. O objetivo desta função é retirar apenas o caminho de um tabuleiro

>>> retiraCaminho t (2,1) Este []
[(Peca Recta 0,(2,1),Este),(Peca (Curva Este) 0,(3,1),Este),(Peca (Rampa Sul) 0,(3,2),Sul),(Peca (Curva Sul) 1,(3,3),Sul),(Peca Recta 1,(2,3),Oeste),(Peca (Curva Oeste) 1,(1,3),Oeste),(Peca (Rampa Sul) 0,(1,2),Norte),(Peca (Curva Norte) 0,(1,1),Norte)]
-}
retiraCaminho :: Tabuleiro -> Posicao -> Orientacao -> [(Peca, Posicao, Orientacao)] -> [(Peca,Posicao, Orientacao)]
retiraCaminho [] pos ori [] = []
retiraCaminho (x:xs) (c,li) orientacao l | elem (posicaoToPeca (x:xs) (c,li) orientacao) l = l
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Recta && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) orientacao) orientacao (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Rampa Norte && orientacao == Norte && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) orientacao) orientacao (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Rampa Norte && orientacao == Sul && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) orientacao) orientacao (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Rampa Sul && orientacao == Norte && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) orientacao) orientacao (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Rampa Sul && orientacao == Sul && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) orientacao) orientacao (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Rampa Oeste && orientacao == Oeste && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) orientacao) orientacao (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Rampa Oeste && orientacao == Este && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) orientacao) orientacao (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Rampa Este && orientacao == Este && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) orientacao) orientacao (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Rampa Este && orientacao == Oeste && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) orientacao) orientacao (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Curva Norte && orientacao == Norte && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) (mudadirOrientacao orientacao)) (mudadirOrientacao orientacao) (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Curva Norte && orientacao == Oeste && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) (mudaesqOrientacao orientacao)) (mudaesqOrientacao orientacao) (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Curva Sul && orientacao == Sul && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) (mudadirOrientacao orientacao)) (mudadirOrientacao orientacao) (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Curva Sul && orientacao == Este && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) (mudaesqOrientacao orientacao)) (mudaesqOrientacao orientacao) (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Curva Este && orientacao == Norte && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) (mudaesqOrientacao orientacao)) (mudaesqOrientacao orientacao) (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Curva Este && orientacao == Este && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) (mudadirOrientacao orientacao)) (mudadirOrientacao orientacao) (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Curva Oeste && orientacao == Oeste && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) (mudadirOrientacao orientacao)) (mudadirOrientacao orientacao) (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | tipo (posicaoToPeca (x:xs) (c,li) orientacao) == Curva Oeste && orientacao == Sul && not (elem (posicaoToPeca (x : xs) (c, li) orientacao) l) = retiraCaminho (x:xs) (move (c,li) (mudaesqOrientacao orientacao)) (mudaesqOrientacao orientacao) (l ++ [(posicaoToPeca (x:xs) (c,li) orientacao)])
                                         | otherwise = l ++ [(Peca Lava 0, (c,li), orientacao)] 


-- | A função __tipo__ recebe um triplo e indica o tipo da peça desse triplo
tipo :: (Peca,Posicao, Orientacao) -> Tipo 
tipo (Peca a x, _,_) = a 


-- | A função __caminhoNLava__ verifica se um caminho é constituído por qualquer peça excepto do tipo "Lava"
caminhoNLava :: [(Peca,Posicao,Orientacao)] -> Bool 
caminhoNLava [] = True
caminhoNLava ((Peca tipo altura, pos, ori):xs) = (tipo /= Lava) && caminhoNLava xs  


-- | A função __mudadirOrientacao__ indica qual a orientação do caminho após uma CurvaDir com direção /n/                                          
mudadirOrientacao :: Orientacao -> Orientacao 
mudadirOrientacao n  | n == Norte = Este 
                     | n == Sul = Oeste
                     | n == Este = Sul 
                     | otherwise = Norte


-- | A função __mudaesqOrientacao__ indica qual a orientação do caminho após uma CurvaEsq com direção /n/
mudaesqOrientacao :: Orientacao -> Orientacao 
mudaesqOrientacao n  | n == Norte = Oeste
                     | n == Sul = Este
                     | n == Este = Norte
                     | otherwise = Sul 


 
{- | A função __posicaoToPeca__ indica-nos qual a peça que se encontra em determinada posição e com determinada orientação num tabuleiro recorrendo às funções auxiliares __aux1__ e __aux2__

>>> posicaoToPeca t (2,1) Norte 
(Peca Recta 0,(2,1),Norte)
-}
posicaoToPeca :: Tabuleiro -> Posicao -> Orientacao -> (Peca,Posicao, Orientacao) 
posicaoToPeca [] (c,l) ori = (Peca Lava 0, (c,l), ori)
posicaoToPeca (x:xs) (c,l) ori  = (aux1 (x:xs) (c,l) 0 0, (c,l), ori)


{- | A função __aux1__ procura se uma peça se encontra em determinada linha, guardando o seu valor em /y/. Caso encontre recorre a __aux2__ a fim de descobrir qual a coluna em que essa peça se encontra

>>> aux1 t (2,1) 0 0 
Peca Recta 0
-}
aux1 :: Tabuleiro -> Posicao -> Int -> Int -> Peca 
aux1 [] (c,l) x y = Peca Lava 0 
aux1 (z:zs) (c,l) x y = if y == l then aux2 z c x 
                                  else aux1 zs (c,l) x (y+1)

{- | A função __aux2__ procura se uma peça se encontra em determinada coluna, guardando o seu valor em /c/ após saber em que linha se encontra.

r = t !! 1

>>> aux2 r 3 1
Peca Recta 0
-}

-- aux2 r (linha onde se encontra a peça com essa coordenada) e vai procurar a peça que se encontra na coluna c  
aux2 :: [Peca] -> Int -> Int -> Peca
aux2 [] n c = Peca Lava 0
aux2 (x:xs) n c = if n == c then x  
                            else aux2 xs n (c+1)                                               


-- | A função __move__ dada uma posição, devolve a seguinte consoante a orientação dada
move :: Posicao -> Orientacao -> Posicao
move (x,y) n | n == Norte = (x, y-1)
             | n == Sul   = (x, y+1)
             | n == Este  = (x+1, y)
             | otherwise  = (x-1, y)


{- | A função __validaNotCaminho__ verifica que tudo o que nao é caminho é do tipo (Peca Lava 0)

>>> validaNotCaminho t (retiraCaminho t (2,1) Este [])
True 
-}
validaNotCaminho :: Tabuleiro -> [(Peca,Posicao, Orientacao)] -> Bool 
validaNotCaminho [] ((peca, pos, ori):xs) = True 
validaNotCaminho t ((peca, pos, ori):xs) = comparaTabuleiro (replaceToLava t (fstAndSnd ((peca,pos,ori):xs)))

                                      
-- | A função __fstAndSnd__ recebe uma lista de triplos e devolve a "Peca" e a respetiva "Posicao" de cada triplo
fstAndSnd :: [(Peca,Posicao,Orientacao)] -> [(Peca,Posicao)] 
fstAndSnd [] = []
fstAndSnd ((a,b,c):xs) = (a,b): fstAndSnd xs  


-- | A função __replaceToLava__  recebe um Tabuleiro e substitui todas as peças do caminho por peças do tipo "Lava"
replaceToLava :: Tabuleiro -> [(Peca, Posicao)] -> Tabuleiro 
replaceToLava t [] = t
replaceToLava t ((peca, (c,l)):xs) = replaceToLava (take l t ++ [take c (t !! l) ++ [Peca Lava 0] ++ drop (c+1) (t !! l)] ++ drop (l+1) t) xs 


-- | A função __comparaTabuleiro__ vai recorrer à função auxiliar __comparaLava__ e aplicá-la a todas as linhas de um tabuleiro  
comparaTabuleiro :: Tabuleiro -> Bool
comparaTabuleiro [] = True
comparaTabuleiro t = foldr ((&&) . comparaLava) True t


-- | A função __comparaLava__ verifica se todas as peças de uma linha do Tabuleiro são todas to tipo "Lava" 
comparaLava :: [Peca] -> Bool
comparaLava [] = True 
comparaLava xs = foldr (\ x -> (&&) (x == Peca Lava 0)) True xs
                                            

-- |A função __segundoElemento__ devolve o elemento que se situa na segunda posição de um triplo 
segundoElemento :: (Peca,Posicao, Orientacao) -> Posicao
segundoElemento (a,b,c) = b


-- |A função __primeiroElemento__ devolve o elemento que se situa na primeira posição de um triplo 
primeiroElemento :: (Peca,Posicao, Orientacao) -> Peca
primeiroElemento (a,b,c) = a


-- | A função __validaProximo__ vai verificar que o caminho ,começando na peça de partida com a orientação inicial, volta a chegar à peça de partida com a mesma orientação com a inicial, recorrendo à função auxiliar __move2__
validaProximo :: [(Peca,Posicao,Orientacao)] -> Bool
validaProximo [] = True
validaProximo (h:t) = move2 (last t) == segundoElemento h 
                    

{- |A função __move2__ , auxiliar da função __validaProximo__ ,vai indicar qual a posição seguinte consoante a posição e orientação atuais

* /Exemplo válido/ 
  
>>> move2 (Peca (Curva Norte) 0, (1,2), Norte)
(2,2)
 
* /Exemplo Inválido/ 
  
>>> move2 (Peca (Curva Norte) 0, (1,2), Este)
(0,0)

-}
move2 :: (Peca,Posicao,Orientacao) -> Posicao
move2 (Peca tipo altura, (x,y) ,n) | tipo == Recta = move (x,y) n
                                   | tipo == Curva Norte && n == Norte = move (x,y) Este 
                                   | tipo == Curva Norte && n == Oeste = move (x,y) Sul
                                   | tipo == Curva Sul   && n == Sul   = move (x,y) Oeste
                                   | tipo == Curva Sul   && n == Este  = move (x,y) Norte
                                   | tipo == Curva Este  && n == Norte = move (x,y) Oeste 
                                   | tipo == Curva Este  && n == Este  = move (x,y) Sul
                                   | tipo == Curva Oeste && n == Oeste = move (x,y) Norte
                                   | tipo == Curva Oeste && n == Sul   = move (x,y) Este
                                   | tipo == Rampa Norte && n == Norte = move (x,y) Norte
                                   | tipo == Rampa Norte && n == Sul   = move (x,y) Sul 
                                   | tipo == Rampa Sul   && n == Norte = move (x,y) Norte
                                   | tipo == Rampa Sul   && n == Sul   = move (x,y) Sul
                                   | tipo == Rampa Este  && n == Este  = move (x,y) Este
                                   | tipo == Rampa Este  && n == Oeste = move (x,y) Oeste
                                   | tipo == Rampa Oeste && n == Oeste = move (x,y) Oeste
                                   | tipo == Rampa Oeste && n == Este  = move (x,y) Este
                                   | otherwise = (0,0)


-- | A função __altAtual__ vai devolver a altura dando uma Peca
altAtual :: Peca -> Int
altAtual (Peca x y) = y  


-- | A função __validaAlturas__ recebe um triplo correspondente a um caminho, um inteiro /n/ que assume o valor da altura inicial e um inteiro /t/ que assume o valor da altura atual
validaAlturas :: [(Peca,Posicao,Orientacao)] -> Int -> Int -> Bool 
validaAlturas [(Peca tipo altura, (c,l), x)] n t | tipo == Recta && t == altura && n == altura = True
                                                 | tipo == Curva Norte && t == altura && n == altura = True
                                                 | tipo == Curva Sul   && t == altura && n == altura = True
                                                 | tipo == Curva Este  && t == altura && n == altura = True
                                                 | tipo == Curva Oeste && t == altura && n == altura = True
                                                 | tipo == Rampa Norte && x == Norte && t == altura && n == (altura + 1)= True
                                                 | tipo == Rampa Norte && x == Sul && (t - 1) == altura && n == altura= True
                                                 | tipo == Rampa Sul   && x == Norte && (t - 1) == altura && n == altura= True
                                                 | tipo == Rampa Sul   && x == Sul && t == altura && n == (altura + 1)= True
                                                 | tipo == Rampa Este  && x == Este && t == altura && n == (altura + 1)= True
                                                 | tipo == Rampa Este  && x == Oeste && (t - 1) == altura && n == altura= True
                                                 | tipo == Rampa Oeste && x == Oeste && t == altura && n == (altura + 1)= True
                                                 | tipo == Rampa Oeste && x == Este && (t - 1) == altura && n == altura= True
                                                 | otherwise = False
validaAlturas ((Peca tipo altura, (c,l), x):tail) n t | tipo == Recta && t == altura = validaAlturas tail n t
                                                      | tipo == Curva Norte && t == altura = validaAlturas tail n t
                                                      | tipo == Curva Sul   && t == altura = validaAlturas tail n t
                                                      | tipo == Curva Este  && t == altura = validaAlturas tail n t
                                                      | tipo == Curva Oeste && t == altura = validaAlturas tail n t
                                                      | tipo == Rampa Norte && x == Norte && t == altura = validaAlturas tail n (t + 1)
                                                      | tipo == Rampa Norte && x == Sul && (t - 1) == altura = validaAlturas tail n altura
                                                      | tipo == Rampa Sul   && x == Norte && (t - 1) == altura = validaAlturas tail n altura
                                                      | tipo == Rampa Sul   && x == Sul && t == altura = validaAlturas tail n (t + 1)
                                                      | tipo == Rampa Este  && x == Este && t == altura = validaAlturas tail n (t + 1)
                                                      | tipo == Rampa Este  && x == Oeste && (t - 1) == altura = validaAlturas tail n altura
                                                      | tipo == Rampa Oeste && x == Oeste && t == altura = validaAlturas tail n (t + 1)
                                                      | tipo == Rampa Oeste && x == Este && (t - 1) == altura = validaAlturas tail n altura
                                                      | otherwise = False


-- | A função __validaAltura__ trata de um caso especial para o caso de quando um caminho tem inicio numa rampa que sobe, recorrendo à função __validaAlturas__ para todos os outros casos
validaAltura :: Mapa -> Bool
validaAltura (Mapa ((c,l), x) t) | (tipo (posicaoToPeca t (c,l) x) == Rampa Sul && x == Norte) || (tipo (posicaoToPeca t (c,l) x) == Rampa Norte && x == Sul) || (tipo (posicaoToPeca t (c,l) x) == Rampa Este && x == Oeste) || (tipo (posicaoToPeca t (c,l) x) == Rampa Oeste && x == Este) = validaAlturas (retiraCaminho t (c,l) x []) (altAtual (primeiroElemento ( posicaoToPeca t (c,l) x) ) +1) (altAtual (primeiroElemento(posicaoToPeca t (c,l) x)) +1) 
                                 | otherwise = validaAlturas (retiraCaminho t (c,l) x []) (altAtual (primeiroElemento ( posicaoToPeca t (c,l) x) ) ) (altAtual (primeiroElemento(posicaoToPeca t (c,l) x)))    