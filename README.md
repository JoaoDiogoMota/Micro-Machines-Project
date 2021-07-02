# Micro-machines

Development of a racing game _Micro-Machines_ in __Haskell__. 

### Features

The main objective of the game is to complete a pre-set route in the shortest possible time. This path can have different heights and is surrounded by lava. The player is penalized whenever he falls on the course or when he falls into the lava. 

#### Assignment 1

The objective of this task is to build a map for the game following a previously registered path. A path consists of a list of steps, where each step is one of the following options: _avança_, _sobe_, _desce_, _curva à esquerda_ or _curva à direita_.

```hs
 type Caminho = [Passo] 
 data Passo = Avanca | Sobe | Desce | CurvaEsq | CurvaDir
```
A map consists of an array of parts (implemented with a list of lists), where each part is at a certain height and can be of the type _lava_, _reta_, _rampa_ or _curva_. Ramp and curve type parts also have an associated orientation, indicating respectively the slope or direction of the curve. It is assumed that the height assigned to a ramp-type piece is its lowest point. All lava parts have a fixed height of 0. 

```hs
type Altura     = Int
data Orientacao = Norte | Sul | Este | Oeste 
data Tipo       = Rampa Orientacao | Curva Orientacao | Reta | Lava 
data Peca       = Peca Tipo Altura
```
The Mapa type is composed of an array of parts (represented by the Tabuleiro type), and an initial position and orientation. Each Posicao on the board is represented by a pair of integers, the position (0,0) corresponding to the upper left corner. 

```hs
type Posicao   = (Int,Int) 
type Tabuleiro = [[Peca]] 
data Mapa      = Mapa (Posicao,Orientacao) Tabuleiro
```

#### Assignment 2

The purpose of this task is to test whether or not a given map is valid according to a set of rules. Consider, for each map, there must be a route of pieces of floor that allows the car to go around the track. 

#### Assignment 3

The objective of this task is to start implementing the game mechanics, specifically the car's movements on the game map and the respective collisions with obstacles. To do this, we start by modeling a Carro as containing a posição, a _direção_ (an angle in degrees) and a _vetor de velocidade_. 

```hs
type Ponto      = (Double,Double) 
type Angulo     = Double 
type Velocidade = (Double,Double) 
type Tempo      = Double 
data Carro      = Carro {
                    posicao :: Ponto, 
                    direcao :: Angulo, 
                    velocidade :: Velocidade
                  }
```
In this task, given the current state of a car, you must calculate its new state after a certain period of time, paying attention to the interactions with the map that may occur during that period, namely:
  * If the car transitions to lava, then it is destroyed;
  * If the car transitions to a position at least one unit lower than the current one, then it crashes and is destroyed;
  * If the car transitions to a position at least one unit higher than the current one, then it collides with the part, and the impact of the collision must be calculated.
  * If the car moves between two positions with a height difference of less than one unit, then it moves normally. 

#### Assignment 4

The objective of this task is to update the game state given the actions performed by a player in a period of time. For this, it is necessary to represent:

* Jogo: The internal state of the game (which must be updated at every moment);
* Ação: Something that indicates, for example, whether the car is accelerating, braking or cornering. 

```hs
data Jogo = Jogo {
              mapa :: Mapa, 
              pista :: Propriedades, 
              carros :: [Carro],
              nitros :: [Tempo],
              historico :: [[Posicao]]
           }

data Propriedades = Propriedades {
                        k_atrito :: Double, 
                        k_pneus :: Double, 
                        k_acel :: Double, 
                        k_peso :: Double, 
                        k_nitro :: Double, 
                        k_roda :: Double,
                    }

```

#### Assignment 5
The objective of this task is to implement the complete game using the library [Gloss](https://hackage.haskell.org/package/gloss).

#### Assignment 6
The purpose of this task is to implement a _bot_ that plays _Micro Machines_ automatically. 


## Collaborators

| Name            	|
|-----------------	|
| [Carolina Cunha](https://github.com/13caroline)  	|
| [João Diogo Mota](https://github.com/JoaoDiogoMota) 	|

> <img src="https://seeklogo.com/images/U/Universidade_do_Minho-logo-CB2F98451C-seeklogo.com.png" align="left" height="48" width="48" > University of Minho, Software Engineering (1st Year).
