-- ===========================================================================================
-- TP Final ALP - Antinori Nicolas
--
-- Compilador de lenguaje Logo
----------------------------------------------------------------
-- AST.hs
-- Este archivo contiene todas las definiciones de datos que vamos a usar para el lenguaje
-- ==========================================================================================

module AST where
 
 import Graphics.GD as GD
 
 -- Tipo para representar los puntos en el plano cartesiano
 type Coord = (Double, Double)
 type IntCoord = (Int, Int)
 
 type Variable = String

 -- Tipo para representar una funcion, los valores de las tuplas corresponden a:
 --  - Nombre
 --  - Lista de argumentos fijos
 --  - Lista de argumentos variables
 --  - Comandos

 type Function = (String, [Variable], [OpArg], [Comm])
 
 type OpArg = (Variable, Val)

 -- Tipos para representar las distintas expresiones y valores de logo

 data Exp = N NumExp | B BoolExp deriving (Show)
 data Val = VN Double | VB Bool deriving (Show)

 -- Este tipo lo usamos para ir guardando todos los puntos a graficar en la imagen

 type ImagePix = (IntCoord, Color)



 -- Tipo de dato "imageDesc" que contiene lo siguiente:
 -- Ip -> Corresponde a un pixel en la imagen
 -- Il -> Corresponde a una linea de Coord a Coord en la imagen

 data ImgDesc = Ip ImagePix
              | Il IntCoord IntCoord Color
              deriving (Show)
 -- Expresiones aritmeticas (Flotantes y/o enteras)
 
 data NumExp = Const Double
             | Var Variable
             | BinOp (Double -> Double -> Double) NumExp NumExp
             | UnOp (Double -> Double) NumExp
             | Div NumExp NumExp

 -- Expresiones Booleanas

 data BoolExp = C Bool
              | VarB Variable 
              | BinOpN (Double -> Double -> Bool) NumExp NumExp
              | BinOpB (Bool -> Bool -> Bool) BoolExp BoolExp
              | Not BoolExp

 -- Comandos

 data Comm = Skip                                           -- No hacemos nada
           | Cond BoolExp [Comm] [Comm]                         -- If
           | Repeat NumExp [Comm]                             -- Primitiva Repeat Logo
           | Make Variable Exp                              -- Primitiva Make Logo (Crear/actualizar variables)
           | LocalMake Variable Exp                         -- Primitiva LocalMake Logo (Crear/actualizar variables)
           | To String [Variable] [(Variable, Exp)] [Comm]    -- Primitiva To Logo (creacion de procedimientos)
           | For (Variable, NumExp) NumExp NumExp [Comm]      -- (1)
           | While BoolExp [Comm]
           | DoWhile [Comm] BoolExp
           | Until BoolExp [Comm]
           | DoUntil [Comm] BoolExp
           | Call String [Exp]                              -- Para llamar a los metodos, [Exp] es la lista de args
           | Forward NumExp                                 -- Primitiva Forward Logo (Da n pasos hacia donde mira)
           | Backward NumExp                                -- Primitiva Backward Logo (Da n pasos hacia atras) 
           | PenDown                                        -- Se apoya el lapiz sobre el "papel" 
           | PenUp                                          -- Se levanta el lapiz
           | NewColor (Either Colors NumExp)                -- Se cambia de color el trazo
           | SetPos NumExp NumExp                           -- Se traslada a la tortuga al punto indicado
           | SetPosX NumExp                                 -- Mueve la tortuga a la coordenada x indicada
           | SetPosY NumExp                                 -- Mueve la tortuga a la coordenada y indicada
           | SetHead NumExp                                 -- La tortuga mira hacia el angulo indicado
           | Home                                           -- Pone la tortuga en el medio de la imagen en angulo 0
           | CanvasSize Int Int
           | BackColor (Either Colors NumExp) 
           | LeftAng NumExp                                 -- Gira la tortuga n angulos hacia la izquierda
           | RightAng NumExp                                -- Gira la tortuga n angulos a la derecha
      deriving (Show)

 -- (1) Lo primero es la variable local seguido del valor inical, el final, los saltos que da y los comandos usados

 -- Colores
 data Colors = Black
             | Blue
             | Green
             | Cyan
             | Red
             | Magenta
             | Yellow
             | White
             | Brown
             | Tan
             | Forest
             | Aqua
             | Salmon
             | Purple
             | Orange
             | Grey
             | Custom Color
         deriving (Show)
   
  -- Instancias show para pruebas

 instance Show NumExp where
   show (Const n) = "Const " ++ show n
   show (Var v) = "Var " ++ show v
   show (BinOp _ e e') = "( " ++ show e ++ " (x) " ++ show e' ++ " )"
   show (Div e e') = "( " ++ show e ++ " / " ++ show e' ++ " )"
   show (UnOp _ e) = "[ (-) " ++ show e ++ " ]"

 instance Show BoolExp where
   show (C b) = show b
   show (BinOpB f e e') = "[ " ++ show e ++ " (B) " ++ show e' ++ " ]"
   show (BinOpN f e e') = "[ " ++ show e ++ " (N) " ++ show e' ++ " ]"
   show (Not e) = "[¬" ++ show e ++ " ]"
   show (VarB v) = show v
