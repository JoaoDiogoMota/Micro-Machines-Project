{-|
Module      : Tarefa4_2017li1g4
Description : Módulo da Tarefa 4 para LI1 17/18

Módulo para a realização da Tarefa 4 de LI1 em 2017/18.
-}
module Tarefa4_2017li1g4 where

import LI11718
import Tarefa3_2017li1g4


{-|
O testes a serem considerados pelo sistema de /feedback/
para a função 'atualiza'.
-}
testesT4 :: [(Tempo,Jogo,Acao)]
testesT4 = [(0.2,Jogo{mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]],pista = Propriedades {k_atrito = 2.0,k_pneus = 3.0,k_acel = 4.0,k_peso = 2.0,k_nitro = 0.0,k_roda = 180.0},carros = [Carro  {posicao  =  (2.5,1.5),direcao  = 0.0,velocidade  =  (1.0,0.0)},Carro  {posicao  =  (3.5,2.5),direcao  =  -90.0,velocidade  =  (0.0,1.0)}],nitros = [],historico = [[],[(2,2)]]}, Acao {acelerar = True, travar = False, esquerda= False, direita= False, nitro = Nothing})]

-- 'e' é o numero do jogador
{-|
Função usada para atualizar o estado do jogo dadas as ações de um jogador num determinado período de tempo.

>>> atualiza 1 jogo1 0 acao 
Jogo {mapa = Mapa ((2,1),Este) [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]], pista = Propriedades {k_atrito = 2.0, k_pneus = 3.0, k_acel = 4.0, k_peso = 2.0, k_nitro = 1.0, k_roda = 180.0}, carros = [Carro {posicao = (1.0,1.0), direcao = 200.0, velocidade = (3.8923868329009514,3.0065601119742285)}], nitros = [], historico = [[(1,1)],[(2,1)]]}
-}
atualiza :: Tempo -> Jogo -> Int -> Acao -> Jogo
atualiza tempo jogo@Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = car,nitros = n, historico = pos} e a = Jogo{mapa = (Mapa((c,l),ori) t),pista = p ,carros = elemento car e newcar,nitros = n, historico = newh}
                                                                                                             where newh = atualizarHistorico pos e (atualizaHistorico (car !! e) (pos !! e))
                                                                                                                   newcar = novoCarro (car !! e) jogo a tempo (tiraPropriedade p) 
 
-- | A função __jogo1__ é um exemplo de um possível 'Jogo'
jogo1 :: Jogo
jogo1 = Jogo {mapa = Mapa ((2,1), Este) t1, pista = p, carros = [carroEXs,carroEX2], nitros = [], historico = [[(2,1)],[(2,1)]] }

-- | A função __jogo2__ é um exemplo de um possível 'Jogo'
jogo2 :: Jogo
jogo2 = Jogo {mapa = Mapa ((2,1), Este) t2, pista = p, carros = [carroEX,carroEX], nitros = [], historico = [[(2,1)],[(2,1)]] }

-- | A função __carroEX__ é um exemplo de um carro possível 
carroEXs :: Carro
carroEXs = Carro {posicao = (4,1), direcao = 20, velocidade = (1,0)}

-- | A função __carroEX2__ é um segundo exemplo de um possível carro
carroEX2 :: Carro
carroEX2 = Carro {posicao = (4,1), direcao = 30, velocidade = (1,0)}

-- | A função __t1__ serve como exemplo de um 'Tabuleiro'
t1 :: Tabuleiro
t1 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Norte) 0,Peca Recta 0,Peca (Curva Este) 0,Peca Lava 0],[Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0,Peca (Rampa Sul) 0,Peca Lava 0],[Peca Lava 0,Peca (Curva Oeste) 1,Peca Recta 1,Peca (Curva Sul) 1,Peca Lava 0],[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- | A função __t2__ serve como um segundo exemplo de um 'Tabuleiro'
t2 :: Tabuleiro 
t2 = [[Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0], [Peca Lava 0,Peca (Rampa Norte) 1,Peca Recta 1,Peca (Rampa Sul) 0,Peca Lava 0], [Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0,Peca Lava 0]]

-- | A função __acao__ é um exemplo de uma possível Ação 
acao :: Acao 
acao = Acao{acelerar=True, travar=False, esquerda=True, direita=False,nitro = Just 1} 

-- | A função __p__ representa um exemplo de propriedades sensatas para um percurso dito normal
p :: Propriedades
p = Propriedades 2 3 4 2 1 180 

{- | A função __atualizaVelocidade__ recebe uma 'Acao', um período de 'Tempo', um 'Tabuleiro', um 'Carro' e as 'Propriedades' desejadas, calculando o novo estado do carrp conforme a junção de todas as propriedades.

>>> atualizaVelocidade acao 1 t1 carroEXs p
Carro {posicao = (1.0,1.0), direcao = 20.0, velocidade = (3.8923868329009514,3.0065601119742285)}
-}   
atualizaVelocidade :: Acao -> Tempo -> Tabuleiro -> Carro -> Propriedades -> Carro 
atualizaVelocidade acao tempo t carro Propriedades{k_atrito = a, k_pneus = p, k_acel = ac, k_peso = ps, k_nitro = n, k_roda = r} = 
    (atualizaNitro tempo (forcaAcel tempo acao (forcaGravidade tempo t (forcaPneus tempo (calculaAtrito tempo carro a) p) ps) ac) acao n)


-- | A função __getVelocidade__ recebe um 'Carro', devolvendo apenas o parâmetro da 'Velocidade'
getVelocidade :: Carro -> Velocidade 
getVelocidade Carro{posicao = (c,l), direcao = x, velocidade = (v1,v2)} = (v1,v2)

{- | A função __calculaAtrito__ recebe um período de 'Tempo', um 'Carro' e um 'Double', correspondente à variável 'k_atrito', calculando o novo estado do carro conforme a sua velocidade e variável k_atrito associada, em que irá atuar o efeito atrito exercido entre o carro e o solo.

>>> calculaAtrito 1 carroEXs 3  
Carro {posicao = (1,1), direcao = 20.0, velocidade = (3.0,0.0)}
-}   
calculaAtrito :: Tempo -> Carro -> Double -> Carro 
calculaAtrito tempo Carro{posicao = (c,l), direcao = x, velocidade = (v1,v2)} a = Carro{posicao = (c,l), direcao = x, velocidade = ((v1 - (v1 * a)) * tempo, (v2 - (v2 * a)) * tempo)} 

{- | A função __forcaPneus__ recebe um período de 'Tempo', um 'Carro' e um 'Double', correspondente à variável 'k_pneus', calculando o novo estado do carro conforme a sua velocidade e variável k_pneus associada, em que irá atuar o atrito exercido pelos pneus.

>>> forcaPneus 2 carroEXs 3  
Carro {posicao = (1.0,1.0), direcao = 20.0, velocidade = (3.4510685758878514,1.4510685758878517)}
-}    
forcaPneus :: Tempo -> Carro -> Double -> Carro 
forcaPneus tempo Carro{posicao = (c,l), direcao = x, velocidade = (v1,v2)} p = Carro{posicao = (c,l), direcao = x, velocidade = ((v1 + norma * cos((45/180)*pi)) * tempo, (v2 + norma * cos((45/180)*pi)) * tempo)}
                                                                        where norma = sin((x/180)*pi) * p

{- | A função __forcaGravidade__ recebe um período de 'Tempo', um 'Tabuleiro', um 'Carro' e um 'Double', correspondente à variável 'k_peso', calculando o novo estado do carro conforme a sua velocidade e variável k_peso associada, sendo que esta é apenas válida quando o 'Carro' se encontra numa Rampa.

>>> forcaGravidade 1 t1 Carro{posicao = (1.5,2.5), direcao = 45, velocidade = (5,4)} 4  
Carro {posicao = (1.5,1.5), direcao = 45.0, velocidade = (9.0,4.0)}
-}    
forcaGravidade :: Tempo -> Tabuleiro -> Carro -> Double -> Carro
forcaGravidade tempo t car@Carro{posicao = (c,l), direcao = x, velocidade = (v1,v2)} ps | eRampa (wherePosition t (c,l)) && tipo(wherePosition t (c,l)) == Rampa Oeste = Carro{posicao = (c,l), direcao = x, velocidade = ((v1 * tempo), (v2 + ps) * tempo)} 
                                                                                        | eRampa (wherePosition t (c,l)) && tipo(wherePosition t (c,l)) == Rampa Este = Carro{posicao = (c,l), direcao = x, velocidade = ((v1 * tempo), (v2 - ps) * tempo)} 
                                                                                        | eRampa (wherePosition t (c,l)) && tipo(wherePosition t (c,l)) == Rampa Norte = Carro{posicao = (c,l), direcao = x, velocidade = ((v1 - ps) * tempo, (v2 * tempo))} 
                                                                                        | eRampa (wherePosition t (c,l)) && tipo(wherePosition t (c,l)) == Rampa Sul = Carro{posicao = (c,l), direcao = x, velocidade = ((v1 + ps) * tempo, (v2 * tempo))}                                                                                      
                                                                                        | otherwise = car                                                                                        


{- | A função __eRampa__ recebe uma 'Peca' e indica se uma peça é do tipo Rampa ou não.

>>> eRampa (Peca (Rampa Norte) 0) 
True

-}
eRampa :: Peca -> Bool 
eRampa (Peca x y) | x == (Rampa Norte) = True   
                  | x == (Rampa Sul) = True  
                  | x == (Rampa Este) = True  
                  | x == (Rampa Oeste) = True                                                                   
                  | otherwise = False  

-- | A função __tipo__ recebe uma 'Peca' e indica o tipo dessa peça
tipo :: Peca -> Tipo
tipo (Peca a x) = a 

{- | A função __forcaAcel__ recebe um período de 'Tempo', uma 'Acao', um 'Carro' e um 'Double', correspondente à variável 'k_acel', calculando o novo estado do carro conforme a sua velocidade e variável k_acel associada.

>>> forcaAcel 1 acao carroEXs 2  
Carro {posicao = (1.0,1.0), direcao = 20.0, velocidade = (2.879385241571817,0.6840402866513374)}
-}                                                                  
forcaAcel :: Tempo -> Acao -> Carro -> Double -> Carro    
forcaAcel tempo Acao{acelerar=a, travar=t, esquerda=e, direita=d,nitro = Just n} Carro{posicao = (c,l), direcao = x, velocidade = (v1,v2)} ac = if (a == True) then Carro{posicao = (c,l), direcao = x, velocidade = (v1 + cos(rad) * tempo * ac, v2 + sin(rad) * tempo * ac)}
                                                                                                                                                else Carro{posicao = (c,l), direcao = x, velocidade = (v1,v2)}
                                                                                                                                              where rad = (x/180)*pi
                                                 

{- | A função __atualizaDirecao__ recebe um 'Carro', uma 'Acao' e um 'Double', correspondente à variável 'k_roda', calculando o novo estado do carro conforme a sua direção e variável k_roda associada.

>>> atualizaDirecao carroEXs acao 90 
Carro {posicao = (1.5,1.5), direcao = 110.0, velocidade = (1.0,0.0)}
-}
atualizaDirecao :: Carro -> Acao -> Double -> Carro  
atualizaDirecao Carro{posicao = (c,l), direcao = x, velocidade = (v1,v2)} a r | esquerda a == True = Carro{posicao = (c,l), direcao = (x + r), velocidade = (v1,v2)}
                                                                              | direita a == True = Carro{posicao = (c,l), direcao = (x - r), velocidade = (v1,v2)}
                                                                              | otherwise = Carro{posicao = (c,l), direcao = x, velocidade = (v1,v2)}


-- | A função __getDirecao__ recebe um 'Carro', devolvendo apenas o parâmetro da 'Direcao'
getDirecao :: Carro -> Angulo 
getDirecao Carro{posicao = (c,l), direcao = x, velocidade = (v1,v2)} = x 


-- | A função __tiraPropriedade__ recebe as 'Propriedades', devolvendo o valor da variável 'k_roda'
tiraPropriedade :: Propriedades -> Double 
tiraPropriedade Propriedades{k_atrito = a, k_pneus = p, k_acel = ac, k_peso = ps, k_nitro = n, k_roda = r} = r

{- | A função __elemento__ altera o carro da posição 'c' da lista que recebe, trocando-o pelo carro 'a'

>>> elemento [carroEXs,carroEX2] 1 Carro{posicao = (2,1), direcao = 20, velocidade = (1,0)}
[Carro {posicao = (1.0,1.0), direcao = 20.0, velocidade = (1.0,0.0)},Carro {posicao = (2.0,1.0), direcao = 20.0, velocidade = (1.0,0.0)}]
-}
elemento :: [Carro] -> Int -> Carro -> [Carro]
elemento carros c a = aux carros 0 c a 
                    where aux :: [Carro] -> Int -> Int -> Carro -> [Carro]
                          aux [] _ _ _ = []
                          aux (x:xs) c l a | c == l = a : xs 
                                           | otherwise = x : (aux xs (c+1) l a)

{- | A função __atualizarHistorico__ altera o historico da posição 'c' da lista 'historico' que recebe, trocando-o pelo historico 'a'

>>> atualizarHistorico [[(2,2),(2,3),(2,4)],[(3,1),(3,2),(3,3)]] 0 [(2,1),(2,2),(2,3),(2,4)]
[[(2,1),(2,2),(2,3),(2,4)],[(3,1),(3,2),(3,3)]]
-}
atualizarHistorico :: [[Posicao]] -> Int -> [Posicao] -> [[Posicao]]
atualizarHistorico historico c a  = aux historico 0 c a
                                  where aux :: [[Posicao]] -> Int -> Int -> [Posicao] -> [[Posicao]]
                                        aux [] _ _ _ = []
                                        aux (x:xs) c l a = if c == l then (a ++ x) : xs
                                                           else x : (aux xs (c+1) l a)


{- | A função __atualizaHistorico__ recebe um 'Carro', uma lista de 'Posicao', atualizando o histórico, adicionando à lista, as posições novas por onde o carro passa.

>>> atualizaHistorico carroEXs [(2,1),(5,2)]
[(1,3),(2,1),(5,2)]
-}
atualizaHistorico :: Carro -> [Posicao] -> [Posicao]
atualizaHistorico carro@Carro{posicao = (c,l), direcao = d, velocidade = (v1,v2)} pos = if elem (floor c, floor l) pos then pos 
                                                                                        else (floor c, floor l) : pos 

{- | A função __atualizaNitro__ recebe um período de 'Tempo', um 'Carro', uma 'Acao' e um 'Double' correspondente à variável 'k_nitro' e devolve um 'Carro' após ser aplicado um nitro.

>>> atualizaNitro 1 carroEXs acao 2
Carro {posicao = (1.0,1.0), direcao = 20.0, velocidade = (1.816164123626784,1.8258905014552553)}
-}
atualizaNitro :: Tempo -> Carro -> Acao -> Double -> Carro 
atualizaNitro tempo carro@Carro{posicao = (c,l), direcao = x, velocidade = (v1,v2)} Acao{acelerar=a, travar=t, esquerda=e, direita=d,nitro = m} n = if m /= Nothing then Carro{posicao = (c,l), direcao = x, velocidade = (v1 + cos(x)* n , v2 + sin(x) * n)}
                                                                                                                                                    else carro 

{- | A função __novoCarro__ recebe um 'Carro', um 'Jogo', uma 'Acao', um 'Tempo', e um 'Double', recebendo assim um 'Carro' com a sua direção e velocidade alteradas

>>> novoCarro carroEXs jogo1 acao 1 2
Carro {posicao = (1.0,1.0), direcao = 22.0, velocidade = (3.4843047710875594,2.093614861246601)}
-}
novoCarro :: Carro -> Jogo -> Acao -> Tempo -> Double -> Carro 
novoCarro carro@Carro{posicao = (c, l), direcao = d, velocidade = (v1,v2)} Jogo{mapa = (Mapa((x,y),ori) t),pista = p ,carros = car,nitros = n, historico = h} a tempo r = Carro {posicao = (c,l), direcao = getDirecao(atualizaDirecao carro a r), velocidade = getVelocidade(atualizaVelocidade a tempo t carro p)}



