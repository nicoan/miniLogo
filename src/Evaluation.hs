-- ===========================================================================================
-- TP Final ALP - Antinori Nicolas
--
-- Compilador de lenguaje Logo
----------------------------------------------------------------
-- Evaluation.hs
-- Este archivo contiene la definicion de la monada que usaremos y los evaluadores monadicos
-- para cada una de las partes del AST
-- ==========================================================================================

module Evaluation where

  import Graphics.GD
  import AST
  import Parser
  import Data.Char

  -- Definimos el entorno
  type Env = [(Variable, Val)]
  type Error = String


  -- Estado Nulo
  initEnv :: Env
  initEnv = []

  -- Definimos el entorno de funciones
  type EnvF = [Function]  

  initEnvF :: EnvF
  initEnvF = []
  
  
  -- El tipo TurtleThings es una tupla con toda la informacion necesaria para el dibujo
  -- La misma contiene:
  --  Posicion de donde se encuentra la tortuga
  --  Angulo de hacia donde mira la tortuga
  --  Color para escribir
  --  Tamaño de la imagen
  --  Si el lapiz esta apoyado o no
  --  Color de fondo
  type TurtleThings = (Coord, Double, Color, (Int, Int), Bool, Color)

  initTT :: TurtleThings
  initTT = ((750.0, 750.0), 90.0, rgb 0 0 0, (1500, 1500), True, rgb 255 255 255)


  -- Imagen inicial (vacia)
  initImg :: [ImgDesc]
  initImg = []


  -- Monada de estado con soporte de errores
  -- Env : Entorno de variables globales
  -- EnvF : Entorno de funciones definidas por el usuario 
  -- TutleThings : Parametros que necesito para ir construyendo la imagen 
  -- InitImg : Descripcion imagen que vamos construyendo a medida que vamos ejecutando el codigo
  -- Env : Lista para debugguar
  newtype State a = State { runState :: Env -> 
                                        EnvF -> 
                                        TurtleThings -> 
                                        [ImgDesc] -> Env -> Either Error (a, Env, EnvF, TurtleThings, [ImgDesc], Env) }



  -- ================================================
  --  Definicion de clases necesarias para la monada
  -- ================================================

  -- Monada con soporte de errores

  class (Monad m) => MonadError m where
    throw :: Error -> m a

  -- Monada de estado
  --  Contiene las funciones:
  --    * update : para actualizar o insertar una nueva variable en el entorno
  --    * lookfor : para buscar una varaible en el entorno

  class (Monad m) => MonadState m where
    update :: Variable -> Val -> m ()
    updateF :: Function -> m ()
    lookfor :: Variable -> m Val
    lookforF :: String -> m Function
    updateDB :: Variable -> Val -> m ()  -- update debug

  -- Monada tortuga
  -- Esta monada es la que contiene informacion necesaria para trazar los dibujos
  -- Contiene las funciones:
  --  setPos : cambia las coordenadas de donde se encuentra la tortuga
  --  getPos : obtiene las coordenadas de donde se encuentra la tortuga
  --  penDown : baja el lapiz para que, cuando la tortuga camine, esta escriba
  --  penUp : sube el lapiz
  --  getPenState : nos dice si el lapiz esta apoyado o no
  --  setCanvasSize : establece el tamaño de la imagen
  --  getCanvasSize : obtiene el tamaño de la imagen
  --  setColor : establece el color con el que se va a escribir en ese momento
  --  getColor : obtenemos el color del lapiz en ese momento
  --  setAngle : establece el angulo de hacia donde mira la tortuga
  --  updateImg : actualizamos la descripcion de la imagen

  class (Monad m) => MonadTurtle m where
    setPos :: Coord -> m ()
    getPos :: m Coord
    penDown :: m ()
    penUp :: m ()
    getPenState :: m Bool
    setCanvasSize :: Int -> Int -> m ()
    getCanvasSize :: m (Int, Int)
    setColor :: Color -> m ()
    getColor :: m Color
    setBackColor :: Color -> m ()
    setAngle :: Double -> m ()
    getAngle :: m Double
    updateImg :: ImgDesc -> m ()

  -- ===============================================
  --           Instancias de la monada
  -- ===============================================

  instance Monad State where
    return x = State (\s sf tt img db -> Right (x, s, sf, tt, img, db))
    m >>= f = State (\s sf tt img db -> let st = runState m s sf tt img db 
                                        in case st of
                                             Left e -> Left e
                                             Right (y, s', sf', tt', img', db') -> runState (f y) s' sf' tt' img' db')


  instance MonadError State where 
    throw e = State (\s sf tt img db -> Left e)

  instance MonadState State where
    update var v = State (\s sf tt img db -> let updateEnv var nv [] = [(var, nv)]
                                                 updateEnv var nv ((var', v):xs) = if (var == var') then (var', nv) : xs
                                                                                   else (var', v) : updateEnv var nv xs
                                             in Right ((), updateEnv var v s, sf, tt, img, db) )
    lookfor var = State (\s sf tt img db -> let look var [] _ _ _ _ _ = Left $ "La variable " ++ var ++ " no esta definida" --(2)
                                                look var ((var', v):xs) s sf tt img db = if ((lowerCase var) == (lowerCase var')) then Right (v, s, sf, tt, img, db) 
                                                                                      else look var xs s sf tt img db
                                            in look var s s sf tt img db)
    updateDB var v = State (\s sf tt img db -> Right ((), s, sf, tt, img, ((var,v):db)) )
  
    lookforF name = State (\s sf tt img db -> let look name [] _ _ _ _ _ = Left $ "El metodo " ++ name ++ " no esta definido." --(2)
                                                  look name ((name', a, a', c):fs) s sf tt img db = if ((lowerCase name') == (lowerCase name)) then Right ((name', a, a', c), s, sf, tt, img, db) 
                                                                                                    else look name fs s sf tt img db
                                              in look name sf s sf tt img db)
    updateF (nom, a, a', c) = State (\s sf tt img db -> let lookupF nom [] = False
                                                            lookupF nom ((nom', _, _, _):xs) = if ((lowerCase nom) == (lowerCase nom')) then True
                                                                                              else lookupF nom xs
                                                            errmsg = "Runtime error, el metodo " ++ nom ++ " ya esta definido."
                                                        in if (lookupF nom sf) then Left errmsg
                                                           else Right ((), s, (nom, a, a', c):sf, tt, img, db) )

  -- (2) A la funcion look le pasamos el entorno entero como argumento para poder devolver directamente Right (v, s)
                                                           
  instance MonadTurtle State where
    setPos xy = State (\s sf tt img db -> let (cord, ang, col, size, pen, bg) = tt
                                          in Right ((), s, sf, (xy, ang, col, size, pen, bg), img, db) )
    getPos = State (\s sf tt img db -> let (cord, ang, col, size, pen, bg) = tt
                                       in Right (cord, s, sf, tt, img, db) )
    penDown = State (\s sf tt img db -> let (cord, ang, col, size, pen, bg) = tt
                                        in Right ((), s, sf, (cord, ang, col, size, True, bg), img, db) )
    penUp = State (\s sf tt img  db-> let (cord, ang, col, size, pen, bg) = tt
                                      in Right ((), s, sf, (cord, ang, col, size, False, bg), img, db) )
    getPenState = State (\s sf tt img db -> let (cord, ang, col, size, pen, bg) = tt
                                            in Right (pen, s, sf, tt, img, db) )
    setCanvasSize n m = State (\s sf tt img db -> let (cord, ang, col, size, pen, bg) = tt
                                                  in Right ((), s, sf, (cord, ang, col, (n, m), pen, bg), img, db)) 
    getCanvasSize = State (\s sf tt img db -> let (cord, ang, col, size, pen, bg) = tt
                                              in Right (size, s, sf, (cord, ang, col, size, pen, bg), img, db) )  
    setColor c = State (\s sf tt img db -> let (cord, ang, col, size, pen, bg) = tt
                                           in Right ((), s, sf, (cord, ang, c, size, pen, bg), img, db) )  
    getColor = State (\s sf tt img db -> let (cord, ang, col, size, pen, bg) = tt
                                         in Right (col, s, sf, tt, img, db) )  
    setBackColor c = State (\s sf tt img db -> let (cord, ang, col, size, pen, bg) = tt
                                               in Right ((), s, sf, (cord, ang, col, size, pen, c), img, db) )  
    setAngle a = State (\s sf tt img db -> let (cord, ang, col, size, pen, bg) = tt
                                           in Right ((), s, sf, (cord, a, col, size, pen, bg), img, db) )  
    getAngle = State (\s sf tt img db -> let (cord, ang, col, size, pen, bg) = tt
                                         in Right (ang, s, sf, tt, img, db) )  
    updateImg pix = State (\s sf tt img db -> Right ((), s, sf, tt, pix:img, db) )
 
  -- Auxiliares

  lowerCase :: String -> String
  lowerCase xs = map toLower xs



  -- ===============================================
  --   Implementacion de los distintos evaluadores
  -- ===============================================

  eval :: [Comm] -> Either Error ([ImgDesc], TurtleThings)
  eval c = case runState (evalCommList c []) initEnv initEnvF initTT initImg [] of
            Left e -> Left e
            Right (_ , _, _, tt, i, _) -> Right (i, tt)

  updateEnv var nv [] = [(var, nv)]
  updateEnv var nv ((var', v):xs) = if ((lowerCase var) == (lowerCase var')) then (var', nv) : xs
                                    else (var', v) : updateEnv var nv xs


  -- Evaluador de comandos
  

  evalCommList :: (MonadError m, MonadState m, MonadTurtle m) => [Comm] -> Env -> m Env
  evalCommList [] localEnv = return localEnv
  evalCommList ((LocalMake var e):cs) localEnv = do val <- evalExp e localEnv
                                                    evalCommList cs (updateEnv var val localEnv) 
  evalCommList (c:cs) localEnv = do evalComm c localEnv
                                    evalCommList cs localEnv


  {- La razon por la que evalCommList devuelve el entorno local es por situaciones como esta:
     
     to test
        localmake "i 0
        while (:i < 10) [
          fd :i
          localmake "i :i + 1
        ]
     end

    Si no devolvieramos el entorno local al evaluar la lista de comandos del while, nunca nos enterariamos del
    cambio en la variable i (pues lanzamos una instancia de evalCommList totalmente distitna a la que ejecuto
    el comando while), por lo tanto nos quedariamos en un bucle infinito

  -}


  -- La lista que le pasamos a como argumento es para los comandos que contienen variables locales
  -- asi hacemos que primero se fijen en las variables locales y luego en las globales 

  evalComm :: (MonadError m, MonadState m, MonadTurtle m) => Comm -> Env -> m ()
  evalComm Skip _ = return ()
  evalComm (Make var e) localEnv = do x <- evalExp e localEnv
                                      update var x
                                      --updateDB var x -- Esto es para el debug
  evalComm (Forward e) localEnv = do VN n <- evalNumExp e localEnv
                                     xy <- getPos
                                     --forward (round n)
                                     forward' (round n) xy
  evalComm (Backward e) localEnv = do VN n <- evalNumExp e localEnv 
                                      backward (round n)
  evalComm (LeftAng e) localEnv = do VN n <- evalNumExp e localEnv
                                     goLeft n
  evalComm (RightAng e )localEnv = do VN n <- evalNumExp e localEnv 
                                      goRight n
  evalComm (SetPos e e') localEnv = let errmsg = "Runtime error, se intenta mover fuera de los limites del tamaño de la imagen."
                                     in do VN x <- evalNumExp e localEnv
                                           VN y <- evalNumExp e' localEnv 
                                           (maxx, maxy) <- getCanvasSize
                                           c <- getColor
                                           if (x > (fromIntegral maxx) || x < 0) then  throw errmsg
                                           else if (y > (fromIntegral maxy) || y < 0) then throw errmsg
                                           else drawLine' (x, y) c

  evalComm (SetPosX e) localEnv = do VN x <- evalNumExp e localEnv 
                                     (_ , y) <- getPos
                                     evalComm (SetPos (Const x) (Const y)) []
  evalComm (SetPosY e) localEnv = do VN y <- evalNumExp e localEnv
                                     (x, _) <- getPos
                                     evalComm (SetPos (Const x) (Const y)) []

  evalComm (SetHead e) localEnv = do VN a <- evalNumExp e localEnv
                                     setAngle (rotate a 0)
  evalComm (Home) _ = do (x, y) <- getCanvasSize
                         evalComm (SetPos (Const ((fromIntegral x) / 2)) (Const ((fromIntegral y) / 2))) []
                         setAngle 90.0

  evalComm (PenDown) _ = penDown
  evalComm (PenUp) _ = penUp
  evalComm (NewColor c) localEnv = do color <- evalColor c localEnv
                                      setColor color
  evalComm (Repeat e coms) localEnv  = let repeat' 0 _ _ _ = return ()   -- La variable rc es el repcount
                                           repeat' n c rc ne = do newLocalEnv <- evalCommList c (updateEnv "repcount" (VN rc) ne)
                                                                  repeat' (n - 1) c (rc + 1) newLocalEnv
                                           errmsg = "Runtime error, repeat: el numero de repeticiones debe ser entero positivo."
                                       in do VN n <- evalNumExp e localEnv 
                                             case (properFraction n) of
                                                  (n', 0.0) -> if (n' >= 0) then repeat' n' coms 0 (("repcount",VN 0.0):localEnv)
                                                               else throw errmsg
                                                  _ -> throw errmsg
  evalComm (Cond b c c') localEnv = do VB tf <- evalBoolExp b localEnv 
                                       if tf then do evalCommList c localEnv
                                                     return ()
                                       else do evalCommList c' localEnv
                                               return ()
  evalComm (For (v,s) e j coms) localEnv = do VN s <- evalNumExp s localEnv 
                                              VN e <- evalNumExp e localEnv
                                              VN j <- evalNumExp j localEnv
                                              if (j == 0.0) then if (s <= e) then evalFor v s e (1.0) coms localEnv
                                                                             else evalFor v s e (-1.0) coms localEnv
                                              else evalFor v s e j coms localEnv
  evalComm (To name a a' c) _ = do argsOp <- evalExps' a' []
                                   updateF (name, a, argsOp, c)
  evalComm (Call n par) localEnv = let err = "Runtime error, la cantidad de argumentos obligatorios para " ++ n ++ " no coincide."
                                   in do (name, a, a', c) <- lookforF n
                                         if (length par < length a) then throw err
                                         else do (obl, op) <- splitAt' (length a) par
                                                 localVals <- evalExps obl [] localEnv
                                                 localOpVals <- evalExps op [] localEnv
                                                 opArgs <- newArgOp localOpVals a' []
                                                 evalCommList c ((zip (map (lowerCase) a) localVals) ++ opArgs)
                                                 return ()
  evalComm (While b c) localEnv = do VB tf <- evalBoolExp b localEnv
                                     if (tf) then do newLocal <- evalCommList c localEnv
                                                     evalComm (While b c) newLocal
                                     else return ()
  evalComm (DoWhile c b) localEnv = do VB tf <- evalBoolExp b localEnv
                                       newLocal <- evalCommList c localEnv
                                       if (tf) then do evalComm (DoWhile c b) newLocal
                                       else return ()
  evalComm (Until b c) localEnv = do VB tf <- evalBoolExp b localEnv
                                     if (tf) then return ()
                                     else do newLocal <- evalCommList c localEnv
                                             evalComm (Until b c) newLocal
  evalComm (DoUntil c b) localEnv = do VB tf <- evalBoolExp b localEnv
                                       newLocal <- evalCommList c localEnv
                                       if (tf) then do return ()
                                       else evalComm (DoUntil c b) newLocal
  evalComm (CanvasSize n m) _ = setCanvasSize n m
  evalComm (BackColor c) _ = do color <- evalColor c []
                                setBackColor color



  -- Funcion que le damos una lista de expresiones a evaluar y nos devuelve una lista de valores
  -- [Exp] --> La lista de expresiones que vamos a ir evaluando
  -- [Val] --> En esta lista vamos a llevar los resultados parciales para devolverlos cuando la lista [Exp] sea vacia
  evalExps :: (MonadError m, MonadState m, MonadTurtle m) => [Exp] -> [Val] -> Env -> m [Val]
  evalExps [] val _ = return val
  evalExps (e:exps) val localEnv = do v <- evalExp e localEnv
                                      evalExps exps (val ++ [v]) localEnv -- (1)



  -- Lo mismo que arriba pero para pares de la forma (Variable, Exp)
  evalExps' :: (MonadError m, MonadState m, MonadTurtle m) => [(Variable,Exp)] -> [(Variable, Val)] -> m [(Variable, Val)]
  evalExps' [] vals = return vals
  evalExps' ((v, e):exps) vals = do val <- evalExp e []
                                    evalExps' exps (vals ++ [(v,val)])

  -- Reemplazamos los valores de los argumentos opcionales
  newArgOp :: (MonadError m, MonadState m, MonadTurtle m) => [Val] -> [(Variable, Val)] -> [(Variable, Val)] -> m [(Variable, Val)]
  newArgOp [] vals newvals = return $ newvals ++ vals 
  newArgOp (newval:xs) ((var, val):vals) newvals = do newArgOp  xs vals ((var, newval) : newvals)
 
  splitAt' :: (MonadError m, MonadState m, MonadTurtle m) => Int -> [a] -> m ([a], [a])
  splitAt' n xs = return (splitAt n xs)

  -- (1) La razon por la que ponemos el argumento al final de la lista, es porque luego, para formar el entorno
  -- de la funcion, zipeamos la lista de los valores ya evaluados con la lista de variables, y de esta forma los
  -- hacemos coincidir de forma correcta.


  evalFor :: (MonadError m, MonadState m, MonadTurtle m) => Variable -> Double -> Double -> Double -> [Comm] ->  Env -> m ()
  evalFor v s e j c localEnv = if (j < 0) then if (s <= e) then return ()  
                                               else do newLocalEnv <- evalCommList c (updateEnv v (VN s) localEnv)
                                                       evalFor v (s + j) e j c newLocalEnv
                               else if (s >= e) then return ()  
                                               else do newLocalEnv <- evalCommList c (updateEnv v (VN s) localEnv)
                                                       evalFor v (s + j) e j c newLocalEnv


  -- Evaluador de expresiones numericas
  
  
  -- La lista que le pasamos a como argumento es para los comandos que contienen variables locales
  -- asi hacemos que primero se fijen en las variables locales y luego en las globales 

  evalNumExp :: (MonadError m, MonadState m, MonadTurtle m) => NumExp -> Env -> m Val
  evalNumExp (Const n) _ = return (VN n)
  evalNumExp (Var x) localEnv =  case lookup (lowerCase x) localEnv of 
                                    Just (VN n) -> return (VN n)
                                    Just (VB b) -> return (VB b)
                                    _ -> lookfor x
  evalNumExp (BinOp f e e') localEnv = do x <- evalNumExp e localEnv
                                          y <- evalNumExp e' localEnv
                                          either throw return (operateNumValBin f x y)
  evalNumExp (UnOp f e) localEnv = do x <- evalNumExp e localEnv
                                      either throw return (operateNumValU f x)
  evalNumExp (Div e e') localEnv  = do x <- evalNumExp e localEnv
                                       y <- evalNumExp e' localEnv
                                       case y of
                                          VN n -> if (n == 0) then throw "Error, division por 0."
                                                  else either throw return (operateNumValBin (/) x y)
                                          _ -> throw "Runtime error, se estan intentando operar dos cosas de distinto tipo"

  operateNumValBin :: (Double -> Double -> Double) -> Val -> Val -> Either Error Val 
  operateNumValBin f (VN n) (VN m) = Right (VN (f n m))
  operateNumValBin f _ _ = Left $ "Runtime error, se estan intentando operar dos cosas de distinto tipo."

  operateNumValU :: (Double -> Double) -> Val -> Either Error Val 
  operateNumValU f (VN n) = Right (VN (f n))
  operateNumValU f _ = Left $ "Runtime error, se estan intentando operar dos cosas de distinto tipo."

  -- Evaluador de expresiones booleanas

  evalBoolExp :: (MonadError m, MonadState m, MonadTurtle m) => BoolExp -> Env -> m Val
  evalBoolExp (C b) _ = return (VB b)
  evalBoolExp (VarB v) localEnv = case lookup (lowerCase v) localEnv of 
                                    Just (VN n) -> return (VN n)
                                    Just (VB b) -> return (VB b)
                                    _ -> lookfor v
  evalBoolExp (BinOpN f e e') localEnv = do x <- evalNumExp e localEnv
                                            y <- evalNumExp e' localEnv
                                            either throw return (operateBoolNVal f x y)
  evalBoolExp (BinOpB f e e') localEnv = do x <- evalBoolExp e localEnv
                                            y <- evalBoolExp e' localEnv
                                            either throw return (operateBoolBVal f x y)
  evalBoolExp (Not e) localEnv = do x <- evalBoolExp e localEnv
                                    case x of 
                                       VB y -> return (VB (not y))
                                       _ -> throw "Runtime error, se estan intentando operar dos cosas de distinto tipo."
 

  operateBoolNVal :: (Double -> Double -> Bool) -> Val -> Val -> Either Error Val 
  operateBoolNVal f (VN n) (VN m) = Right (VB (f n m))
  operateBoolNVal f _ _ = Left $ "Runtime error, se estan intentando operar dos cosas de distinto tipo."

  operateBoolBVal :: (Bool -> Bool -> Bool) -> Val -> Val -> Either Error Val 
  operateBoolBVal f (VB p) (VB q) = Right (VB (f p q))
  operateBoolBVal f _ _ = Left $ "Runtime error, se estan intentando operar dos cosas de distinto tipo."

  -- Evaluador de expresiones generales (booleanos, numericos, etc)

  evalExp :: (MonadError m, MonadState m, MonadTurtle m) => Exp -> Env -> m Val
  evalExp (N e) env = evalNumExp e env
  evalExp (B e) env = evalBoolExp e env


  -- Comandos para manejar la grafica
  
  -- Evaluador de colores 
  evalColor :: (MonadError m, MonadState m, MonadTurtle m) => Either Colors NumExp -> Env -> m Color
  evalColor (Left Black) _ = return (rgb 0 0 0)
  evalColor (Left Blue) _ = return (rgb 0 0 255 )
  evalColor (Left Green) _ = return (rgb 0 255 0)
  evalColor (Left Cyan) _ = return (rgb 0 255 255)
  evalColor (Left Red)  _= return (rgb 255 0 0)
  evalColor (Left Magenta) _ = return (rgb 255 0 255)
  evalColor (Left Yellow) _ = return (rgb 255 255 0)
  evalColor (Left White) _ = return (rgb 255 255 255)
  evalColor (Left Brown) _ = return (rgb 165 42 42)
  evalColor (Left Tan)  _ = return (rgb 210 180 140)
  evalColor (Left Forest) _ = return (rgb 0 100 0)
  evalColor (Left Aqua) _ = return (rgb 127 255 212)
  evalColor (Left Salmon) _ = return (rgb 250 128 114)
  evalColor (Left Purple) _ = return (rgb 160 32 240)
  evalColor (Left Orange) _ = return (rgb 255 165 0)
  evalColor (Left Grey)  _ = return (rgb 128 128 128)
  evalColor (Left (Custom c)) _ = return c
  evalColor (Right n) localEnv = do VN m <- evalNumExp n localEnv
                                    case floor m of
                                          0 -> return (rgb 0 0 0)
                                          1 -> return (rgb 0 0 255)
                                          2 -> return (rgb 0 255 0)
                                          3 -> return (rgb 0 255 255)
                                          4 -> return (rgb 255 0 0)
                                          5 -> return (rgb 255 0 255)
                                          6 -> return (rgb 255 255 0)
                                          7 -> return (rgb 255 255 255)
                                          8 -> return (rgb 165 42 42)
                                          9 -> return (rgb 210 180 140)   
                                          10 -> return (rgb 0 100 0)
                                          11 -> return (rgb 127 255 212)
                                          12 -> return (rgb 250 128 114)
                                          13 -> return (rgb 160 32 240)
                                          14 -> return (rgb 255 165 0)
                                          15 -> return (rgb 128 128 128)
                                          _ -> return (rgb 0 0 0)

  -- Damos n pasos hacia adelante

  makeStep :: (MonadError m, MonadState m, MonadTurtle m) => Int -> m ()
  makeStep 0 = return ()
  makeStep n = do c <- getColor
                  drawDot c
                  makeStep (n - 1)

  forward :: (MonadError m, MonadState m, MonadTurtle m) => Int -> m ()
  forward 0 = do makeStep 1
                 return ()
  forward n = if (n < 0) then do a <- getAngle
                                 setAngle (rotate 180.0 a)
                                 makeStep (-n)
                                 setAngle (rotate 180.0 a)
              else makeStep n

  backward :: (MonadError m, MonadState m, MonadTurtle m) => Int -> m ()
  backward n = do forward (-n)
                  a <- getAngle
                  setAngle (rotate 180.0 a)


  goLeft :: (MonadError m, MonadState m, MonadTurtle m) => Double -> m ()
  goLeft n = do a <- getAngle
                setAngle (rotate n a)
  
  goRight :: (MonadError m, MonadState m, MonadTurtle m) => Double -> m ()
  goRight n = do a <- getAngle
                 setAngle (rotate (- n) a)


  -- Funcion para escribir un punto en nuestra representacion de la imagen

  drawDot :: (MonadError m, MonadState m, MonadTurtle m) => Color -> m ()
  drawDot c = let roundtuple (x, y) = (round x, round y)
              in do xy <- getPos
                    c <- getColor
                    a <- getAngle
                    pen <- getPenState
                    wz <- nextDot a xy
                    setPos wz
                    if (pen) then updateImg (Ip ((roundtuple wz), c))
                    else return ()

  -- Funcion para dibujar una linea desde donde nos encontramos hasta coord en nuestra representacion de la img

  drawLine' :: (MonadError m, MonadState m, MonadTurtle m) => Coord -> Color -> m ()
  drawLine' wz c = let roundtuple (x, y) = (round x, round y)
                   in do xy <- getPos
                         c <- getColor
                         pen <- getPenState
                         setPos wz
                         if (pen) then updateImg (Il (roundtuple xy) (roundtuple wz) c)
                         else return ()

  -- Lo mismo que arriba, pero en lugar de tomar la posicion final, toma la inicial

  drawLine'' :: (MonadError m, MonadState m, MonadTurtle m) => Coord -> Color -> m ()
  drawLine'' xy c = let roundtuple (x, y) = (round x, round y)
                    in do wz <- getPos
                          c <- getColor
                          pen <- getPenState
                          setPos wz
                          if (pen) then updateImg (Il (roundtuple xy) (roundtuple wz) c)
                          else return ()

  -- Optimizacion de la funcion forward: En lugar de dibujar el camino punto a punto, traza una linea desde el inicio
  -- al fin del camino.
  forward' :: (MonadError m, MonadState m, MonadTurtle m) => Int => Coord => m()
  forward' 0 xy = do c <- getColor
                     drawLine'' xy c
                     return ()
  forward' n xy = do a <- getAngle
                     ab <- getPos
                     wz <- nextDot a ab
                     setPos wz
                     forward' (n - 1) xy
                    


  -- Funcion que nos calcula hacia que direccion ira el siguiente punto
  -- a : Angulo hacia donde miro
  -- (x, y) : Posicion en la que me encuentro

  nextDot :: (MonadError m, MonadState m, MonadTurtle m) => Double -> Coord -> m Coord
  nextDot a (x, y) = let (w, z) = ((cos (toRadian a)), - (sin (toRadian a)))
                     in return (x + w, y + z)



  toRadian :: Double -> Double
  toRadian x = x/180*pi


  --Funcion para rotar el angulo
  -- x -> Cuanto rotar
  -- a -> Angulo a rotar
  rotate :: Double -> Double -> Double
  rotate x a = if (n > 360.0) then resta360 n 
               else if (n < -360.0) then suma360 n
               else n
                    where n = x + a

  resta360 :: (Num a, Ord a) => a -> a
  resta360 n = if (n > 360) then resta360 (n - 360)
               else n

  suma360 :: (Num a, Ord a) => a -> a
  suma360 n = if (n < -360) then resta360 (360 + n)
              else n
