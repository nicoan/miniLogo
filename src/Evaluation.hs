{-# LANGUAGE FlexibleContexts #-}
-- ===========================================================================================
-- TP Final ALP - Antinori Nicolas
--
-- Compilador de lenguaje Logo
----------------------------------------------------------------
-- Evaluation.hs
-- Este archivo contiene la definicion de la monada que usaremos y los evaluadores monadicos
-- para cada una de las partes del AST
-- ==========================================================================================

module Evaluation (CompiledImage (..), eval) where
  import Control.Monad.State (StateT, runStateT, get, put)
  import Control.Monad.Except (MonadError, Except, runExcept, throwError)
  import Data.List (find)
  import Data.Fixed (mod')
  import Data.Char (toLower)
  import Graphics.GD (Color, rgb)
  import AST

  -- Variables environment
  type EnvironmentVariables = [(Variable, Val)]

  -- Functions environment
  type EnvironmentFunctions = [Function]

  data ImageState = ImageState {
    penPosition :: Coord,
    angle :: Double,
    penColor :: Color,
    backgroundColor:: Color,
    size :: ImageSize,
    isDrawing :: Bool,
    description :: ImageDescription
  }

  initialImageState :: ImageState
  initialImageState = ImageState {
    penPosition = (750.0, 750.0),
    angle = 90.0,
    penColor = rgb 0 0 0,
    backgroundColor = rgb 255 255 255,
    size = (1500, 1500),
    isDrawing = True,
    description = []
  }

  data CompilerState = CompilerState {
    environment :: EnvironmentVariables,
    environmentFunctions :: EnvironmentFunctions,
    imageState :: ImageState
  }

  type CompilationState = StateT CompilerState (Except String)

  initialState :: CompilerState
  initialState = CompilerState {
    environment = [],
    environmentFunctions = [],
    imageState = initialImageState
  }

  data CompiledImage = CompiledImage {
    cBackgroundColor :: Color,
    cSize :: ImageSize,
    cDescription :: ImageDescription
  }

  -- ================================================
  --  Definicion de clases necesarias para la monada
  -- ================================================
  -- Monada de estado
  --  Contiene las funciones:
  --    * update : para actualizar o insertar una nueva variable en el entorno
  --    * lookfor : para buscar una varaible en el entorno

  class (Monad m, MonadError String m) => MonadEvaluation m where
    update :: Variable -> Val -> m ()
    defineFunction :: Function -> m ()
    lookfor :: Variable -> m Val
    lookForFunction :: String -> m Function

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

  class (Monad m, MonadError String m) => MonadImageState m where
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
    updateImg :: ImageLine -> m ()

  -- ===============================================
  --           Instancias de la monada
  -- ===============================================

    -- Auxiliares

  lowerCase :: String -> String
  lowerCase xs = map toLower xs


  updateEnv :: [Char] -> t -> [(String, t)] -> [(String, t)]
  updateEnv var nv [] = [(var, nv)]
  updateEnv var nv ((var', v):xs) = if ((lowerCase var) == (lowerCase var')) then (var', nv) : xs
                                    else (var', v) : updateEnv var nv xs

  instance MonadEvaluation CompilationState where
    update var v = do
      currentState <- get
      let currentEnvironment = environment currentState
      put $ currentState { environment = updateEnv var v currentEnvironment }
      return ()

    lookfor var = do
      currentState <- get
      let currentEnvironment = environment currentState
      let foundVariable = find (\(vName, _) -> (lowerCase vName) == (lowerCase var)) currentEnvironment
      case foundVariable of
        Nothing -> throwError $ "Variable \"" ++ var ++ "\" is not defined"
        Just (_, val) -> return val

    lookForFunction fName = do
      currentState <- get
      let currentEnvironment = environmentFunctions currentState
      let function = find (\function -> (lowerCase $ name function) == (lowerCase fName)) currentEnvironment
      case function of
        Nothing -> throwError $ "Function \"" ++ fName ++ "\" is not defined."
        Just f -> return f

    defineFunction function = do
      currentState <- get
      let currentEnvironment = environmentFunctions currentState
      let fName = name function
      let mFunction = find (\function -> (lowerCase $ name function) == (lowerCase fName)) currentEnvironment
      case mFunction of
        Nothing -> put $ currentState { environmentFunctions = function:currentEnvironment }
        Just _ -> throwError $ "Function \"" ++ fName ++ "\" is already defined."


  instance MonadImageState (CompilationState) where
    setPos xy = do
      currentState <- get
      let currentImageState = imageState currentState
      let newImageState = currentImageState { penPosition = xy }
      put $ currentState { imageState = newImageState }
      return ()

    getPos = do
      currentState <- get
      let currentImageState = imageState currentState
      return $ penPosition currentImageState

    penDown = do
      currentState <- get
      let currentImageState = imageState currentState
      let newImageState = currentImageState { isDrawing = True }
      put $ currentState { imageState = newImageState }
      return ()

    penUp = do
      currentState <- get
      let currentImageState = imageState currentState
      let newImageState = currentImageState { isDrawing = False }
      put $ currentState { imageState = newImageState }
      return ()

    getPenState = do
      currentState <- get
      let currentImageState = imageState currentState
      return $ isDrawing currentImageState

    setCanvasSize n m  = do
      currentState <- get
      let currentImageState = imageState currentState
      let newImageState = currentImageState { size = (n, m) }
      put $ currentState { imageState = newImageState }
      return ()

    getCanvasSize = do
      currentState <- get
      let currentImageState = imageState currentState
      return $ size currentImageState

    setColor c = do
      currentState <- get
      let currentImageState = imageState currentState
      let newImageState = currentImageState { penColor = c }
      put $ currentState { imageState = newImageState }
      return ()

    getColor = do
      currentState <- get
      let currentImageState = imageState currentState
      return $ penColor currentImageState

    setBackColor c = do
      currentState <- get
      let currentImageState = imageState currentState
      let newImageState = currentImageState { backgroundColor = c }
      put $ currentState { imageState = newImageState }
      return ()

    setAngle a = do
      currentState <- get
      let currentImageState = imageState currentState
      let newImageState = currentImageState { angle = a }
      put $ currentState { imageState = newImageState }
      return ()

    getAngle = do
      currentState <- get
      let currentImageState = imageState currentState
      return $ angle currentImageState

    updateImg line = do
      currentState <- get
      let currentImageState = imageState currentState
      let currentImageDescription = description currentImageState
      let newImageState = currentImageState { description = line:currentImageDescription }
      put $ currentState { imageState = newImageState }
      return ()


  -- ===============================================
  --   Implementacion de los distintos evaluadores
  -- ===============================================

  eval :: [Statement] -> Either [Char] CompiledImage
  eval statements = let evalStmList s = runExcept . flip runStateT initialState $ evalStatementList s []
                    in case (evalStmList statements) of
                      Left err -> Left err
                      Right (_, state) -> let imgState = imageState state
                                              imgDescription = description imgState
                                              imgSize = size imgState
                                              imgBackColor = backgroundColor imgState
                                          in Right CompiledImage {
                                            cBackgroundColor = imgBackColor,
                                            cSize = imgSize,
                                            cDescription = imgDescription
                                          }


  -- Evaluador de comandos


  evalStatementList :: (MonadEvaluation m, MonadImageState m) => [Statement] -> EnvironmentVariables -> m EnvironmentVariables
  evalStatementList [] localEnv = return localEnv
  evalStatementList ((LocalMake var e):cs) localEnv = do val <- evalExp e localEnv
                                                         evalStatementList cs (updateEnv var val localEnv)
  evalStatementList (c:cs) localEnv = do evalStatement c localEnv
                                         evalStatementList cs localEnv


  {- La razon por la que evalStatementList devuelve el entorno local es por situaciones como esta:

     to test
        localmake "i 0
        while (:i < 10) [
          fd :i
          localmake "i :i + 1
        ]
     end

    Si no devolvieramos el entorno local al evaluar la lista de comandos del while, nunca nos enterariamos del
    cambio en la variable i (pues lanzamos una instancia de evalStatementList totalmente distitna a la que ejecuto
    el comando while), por lo tanto nos quedariamos en un bucle infinito

  -}


  -- La lista que le pasamos a como argumento es para los comandos que contienen variables locales
  -- asi hacemos que primero se fijen en las variables locales y luego en las globales

  evalStatement :: (MonadEvaluation m, MonadImageState m) => Statement -> EnvironmentVariables -> m ()
  evalStatement Skip _ = return ()

  evalStatement (LocalMake n _) _ = throwError $ "Trying to declare a local variable \"" ++ n ++ "\" in global scope."

  evalStatement (Make var e) localEnv = do
    x <- evalExp e localEnv
    update var x

  evalStatement (Forward e) localEnv = do
    VN n <- evalNumExp e localEnv
    forward (round n)

  evalStatement (Backward e) localEnv = do
    VN n <- evalNumExp e localEnv
    backward (round n)

  evalStatement (LeftAng e) localEnv = do
    VN n <- evalNumExp e localEnv
    goLeft n

  evalStatement (RightAng e )localEnv = do
    VN n <- evalNumExp e localEnv
    goRight n

  evalStatement (SetPos e e') localEnv =
    let errmsg cName cVal = "Trying to move out of image limits (" ++ cName ++ " = " ++ cVal ++ ")"
    in do VN x <- evalNumExp e localEnv
          VN y <- evalNumExp e' localEnv
          (maxx, maxy) <- getCanvasSize
          if (x > (fromIntegral maxx) || x < 0) then throwError $ errmsg "x" (show x)
          else if (y > (fromIntegral maxy) || y < 0) then throwError $ errmsg "y" (show y)
          else drawLine (x, y)

  evalStatement (SetPosX e) localEnv = do
    VN x <- evalNumExp e localEnv
    (_ , y) <- getPos
    evalStatement (SetPos (Const x) (Const y)) []

  evalStatement (SetPosY e) localEnv = do
    VN y <- evalNumExp e localEnv
    (x, _) <- getPos
    evalStatement (SetPos (Const x) (Const y)) []

  evalStatement (SetHead e) localEnv = do
    VN a <- evalNumExp e localEnv
    setAngle (rotate a 0)

  evalStatement (Home) _ = do
    (x, y) <- getCanvasSize
    evalStatement (SetPos (Const ((fromIntegral x) / 2)) (Const ((fromIntegral y) / 2))) []
    setAngle 90.0

  evalStatement (PenDown) _ = penDown

  evalStatement (PenUp) _ = penUp

  evalStatement (NewColor c) localEnv = do
    color <- evalColor c localEnv
    setColor color

  evalStatement (Repeat e coms) localEnv  =
    let repeat' 0 _ _ _ = return ()   -- La variable rc es el repcount
        repeat' n c rc ne = do newLocalEnv <- evalStatementList c (updateEnv "repcount" (VN rc) ne)
                               repeat' (n - 1) c (rc + 1) newLocalEnv
        errmsg n = "Repeat: repetition number must be positive (n = " ++ n ++ ")"
    in do VN n <- evalNumExp e localEnv
          case (properFraction n) of
            (n', 0.0) -> if (n' >= 0) then repeat' n' coms 0 (("repcount", VN 0.0):localEnv)
                         else throwError $ errmsg $ show n'
            _ -> throwError $ errmsg $ show $ round n

  evalStatement (If b c c') localEnv = do
    VB tf <- evalBoolExp b localEnv
    if tf then do _ <- evalStatementList c localEnv
                  return ()
    else do _ <- evalStatementList c' localEnv
            return ()

  evalStatement (For (v, s) e j coms) localEnv =
    do VN s' <- evalNumExp s localEnv
       VN e' <- evalNumExp e localEnv
       VN j' <- evalNumExp j localEnv
       if (j' == 0.0) then if (s' <= e') then evalFor v s' e' (1.0) coms localEnv
                           else evalFor v s' e' (-1.0) coms localEnv
       else evalFor v s' e' j' coms localEnv

  evalStatement (To n a a' c) _ = do
    argsOp <- evalExps' a' []
    defineFunction Function {
      name = n,
      arguments = a,
      optionalArguments = argsOp,
      body = c
    }

  evalStatement (Call fName par) localEnv = do
    function <- lookForFunction fName
    let args = arguments function
    let optArgs = optionalArguments function
    let fBody = body function
    if (length par < length args) then throwError $ "Too few arguments for function \"" ++ fName ++ "\"."
    else do let (obl, op) = splitAt (length args) par
            localVals <- evalExps obl [] localEnv
            localOpVals <- evalExps op [] localEnv
            opArgs <- newArgOp localOpVals optArgs []
            _ <- evalStatementList fBody ((zip (map (lowerCase) args) localVals) ++ opArgs)
            return ()

  evalStatement (While b c) localEnv = do
    VB tf <- evalBoolExp b localEnv
    if (tf) then do newLocal <- evalStatementList c localEnv
                    evalStatement (While b c) newLocal
    else return ()

  evalStatement (DoWhile c b) localEnv = do
    VB tf <- evalBoolExp b localEnv
    newLocal <- evalStatementList c localEnv
    if (tf) then do evalStatement (DoWhile c b) newLocal
    else return ()

  evalStatement (Until b c) localEnv = do
    VB tf <- evalBoolExp b localEnv
    if (tf) then return ()
    else do newLocal <- evalStatementList c localEnv
            evalStatement (Until b c) newLocal

  evalStatement (DoUntil c b) localEnv = do
    VB tf <- evalBoolExp b localEnv
    newLocal <- evalStatementList c localEnv
    if (tf) then do return ()
    else evalStatement (DoUntil c b) newLocal

  evalStatement (CanvasSize n m) _ = setCanvasSize n m

  evalStatement (BackColor c) _ = do
    color <- evalColor c []
    setBackColor color



  -- Funcion que le damos una lista de expresiones a evaluar y nos devuelve una lista de valores
  -- [Exp] --> La lista de expresiones que vamos a ir evaluando
  -- [Val] --> En esta lista vamos a llevar los resultados parciales para devolverlos cuando la lista [Exp] sea vacia
  evalExps :: (MonadEvaluation m, MonadImageState m) => [Exp] -> [Val] -> EnvironmentVariables -> m [Val]
  evalExps [] val _ = return val
  evalExps (e:exps) val localEnv = do v <- evalExp e localEnv
                                      evalExps exps (val ++ [v]) localEnv -- (1)



  -- Lo mismo que arriba pero para pares de la forma (Variable, Exp)
  evalExps' :: (MonadEvaluation m, MonadImageState m) => [(Variable,Exp)] -> EnvironmentVariables -> m EnvironmentVariables
  evalExps' [] vals = return vals
  evalExps' ((v, e):exps) vals = do val <- evalExp e []
                                    evalExps' exps (vals ++ [(v,val)])

  -- Reemplazamos los valores de los argumentos opcionales
  newArgOp :: (MonadEvaluation m, MonadImageState m) => [Val] -> EnvironmentVariables -> EnvironmentVariables -> m EnvironmentVariables
  newArgOp (_:_) [] _ = return []
  newArgOp [] vals newvals = return $ newvals ++ vals
  newArgOp (newval:xs) ((var, _):vals) newvals = do newArgOp  xs vals ((var, newval) : newvals)

  -- (1) La razon por la que ponemos el argumento al final de la lista, es porque luego, para formar el entorno
  -- de la funcion, zipeamos la lista de los valores ya evaluados con la lista de variables, y de esta forma los
  -- hacemos coincidir de forma correcta.


  evalFor :: (MonadEvaluation m, MonadImageState m) => Variable -> Double -> Double -> Double -> [Statement] ->  EnvironmentVariables -> m ()
  evalFor v s e j c localEnv = if (j < 0) then if (s <= e) then return ()
                                               else do newLocalEnv <- evalStatementList c (updateEnv v (VN s) localEnv)
                                                       evalFor v (s + j) e j c newLocalEnv
                               else if (s >= e) then return ()
                                               else do newLocalEnv <- evalStatementList c (updateEnv v (VN s) localEnv)
                                                       evalFor v (s + j) e j c newLocalEnv


  -- Evaluador de expresiones numericas


  -- La lista que le pasamos a como argumento es para los comandos que contienen variables locales
  -- asi hacemos que primero se fijen en las variables locales y luego en las globales

  typeMismatchError :: String
  typeMismatchError = "Tried to operate a boolean value with a numeric value."

  evalNumExp :: (MonadEvaluation m, MonadImageState m) => NumExp -> EnvironmentVariables -> m Val
  evalNumExp (Const n) _ = return (VN n)
  evalNumExp (Var x) localEnv =  case lookup (lowerCase x) localEnv of
                                    Just (VN n) -> return (VN n)
                                    Just (VB b) -> return (VB b)
                                    _ -> lookfor x

  evalNumExp (BinOp f e e') localEnv = do x <- evalNumExp e localEnv
                                          y <- evalNumExp e' localEnv
                                          either throwError return (operateNumValBin f x y)

  evalNumExp (UnOp f e) localEnv = do x <- evalNumExp e localEnv
                                      either throwError return (operateNumValU f x)

  evalNumExp (Div e e') localEnv  = do x <- evalNumExp e localEnv
                                       y <- evalNumExp e' localEnv
                                       case y of
                                          VN n -> if (n == 0) then throwError "Divided by zero."
                                                  else either throwError return (operateNumValBin (/) x y)
                                          _ -> throwError typeMismatchError


  operateNumValBin :: (Double -> Double -> Double) -> Val -> Val -> Either String Val
  operateNumValBin f (VN n) (VN m) = Right (VN (f n m))
  operateNumValBin _ _ _ = Left typeMismatchError


  operateNumValU :: (Double -> Double) -> Val -> Either String Val
  operateNumValU f (VN n) = Right (VN (f n))
  operateNumValU _ _ = Left $ typeMismatchError


  evalBoolExp :: (MonadEvaluation m, MonadImageState m) => BoolExp -> EnvironmentVariables -> m Val
  evalBoolExp (C b) _ = return (VB b)
  evalBoolExp (VarB v) localEnv = case lookup (lowerCase v) localEnv of
                                    Just (VN n) -> return (VN n)
                                    Just (VB b) -> return (VB b)
                                    _ -> lookfor v
  evalBoolExp (BinOpN f e e') localEnv = do x <- evalNumExp e localEnv
                                            y <- evalNumExp e' localEnv
                                            either throwError return (operateBoolNVal f x y)
  evalBoolExp (BinOpB f e e') localEnv = do x <- evalBoolExp e localEnv
                                            y <- evalBoolExp e' localEnv
                                            either throwError return (operateBoolBVal f x y)
  evalBoolExp (Not e) localEnv = do x <- evalBoolExp e localEnv
                                    case x of
                                       VB y -> return (VB (not y))
                                       _ -> throwError typeMismatchError


  operateBoolNVal :: (Double -> Double -> Bool) -> Val -> Val -> Either String Val
  operateBoolNVal f (VN n) (VN m) = Right (VB (f n m))
  operateBoolNVal _ _ _ = Left typeMismatchError


  operateBoolBVal :: (Bool -> Bool -> Bool) -> Val -> Val -> Either String Val
  operateBoolBVal f (VB p) (VB q) = Right (VB (f p q))
  operateBoolBVal _ _ _ = Left typeMismatchError


  -- Evaluador de expresiones generales (booleanos, numericos, etc)
  evalExp :: (MonadEvaluation m, MonadImageState m) => Exp -> EnvironmentVariables -> m Val
  evalExp (N e) env = evalNumExp e env
  evalExp (B e) env = evalBoolExp e env


  -- Comandos para manejar la grafica

  -- Evaluador de colores
  evalColor :: (MonadEvaluation m, MonadImageState m) => Either Colors NumExp -> EnvironmentVariables -> m Color
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


  forward :: (MonadEvaluation m, MonadImageState m) => Int -> m ()
  forward n = do a <- getAngle
                 ab <- getPos
                 wz <- nextPosition a n ab
                 drawLine wz
                 return ()


  backward :: (MonadEvaluation m, MonadImageState m) => Int -> m ()
  backward n = do a <- getAngle
                  setAngle (rotate 180.0 a)
                  a' <- getAngle
                  ab <- getPos
                  wz <- nextPosition a' n ab
                  drawLine wz
                  setAngle a
                  return ()


  goLeft :: (MonadEvaluation m, MonadImageState m) => Double -> m ()
  goLeft n = do a <- getAngle
                setAngle (rotate n a)


  goRight :: (MonadEvaluation m, MonadImageState m) => Double -> m ()
  goRight n = do a <- getAngle
                 setAngle (rotate (- n) a)


  -- Funcion para dibujar una linea desde donde nos encontramos hasta coord en nuestra representacion de la img
  drawLine :: (MonadEvaluation m, MonadImageState m) => Coord -> m ()
  drawLine wz = let roundtuple (x, y) = (round x, round y)
                in do xy <- getPos
                      c <- getColor
                      pen <- getPenState
                      setPos wz
                      if (pen) then
                        updateImg ImageLine {
                          startPosition = roundtuple xy,
                          endPosition = roundtuple wz,
                          lineColor = c
                        }
                      else return ()



  -- Funcion que nos calcula hacia que direccion ira el siguiente punto
  -- a : Angulo hacia donde miro
  -- n : Numero de pasos
  -- (x, y) : Posicion en la que me encuentro
  nextPosition :: (MonadEvaluation m, MonadImageState m) => Double -> Int -> Coord -> m Coord
  nextPosition a n (x, y) = let steps = fromIntegral n
                                (w, z) = (steps * (cos (toRadian a)), - steps * (sin (toRadian a)))
                            in return (x + w, y + z)


  toRadian :: Double -> Double
  toRadian x = x/180*pi

  rotate :: Double -> Double -> Double
  rotate x a = (x + a) `mod'` 360
