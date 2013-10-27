-- ===========================================================================================
-- TP Final ALP - Antinori Nicolas
--
-- Compilador de lenguaje Logo
----------------------------------------------------------------
-- Parser.hs
-- Este archivo contiene el parser del lenguaje
-- ==========================================================================================

module Parser(P.parse, logoTokens, parseLogoDoc) where

  import Text.ParserCombinators.Parsec.Language
  import Text.ParserCombinators.Parsec hiding (parse)
  import qualified Text.ParserCombinators.Parsec as P
  import Text.ParserCombinators.Parsec.Token
  import AST
  import Graphics.GD
  import GHC.Float


  logoStyle :: LanguageDef st
  logoStyle = emptyDef
               { commentStart   = "/*"
               , commentEnd     = "*/"
               , commentLine    = ";"
               , nestedComments = True
               , identStart     = char ':' <|> letter                   --Para invocar el valor de una variable siempre preceden primero los dos puntos
               , identLetter    = alphaNum <|> oneOf "_'."
               , opStart    = opLetter haskellStyle
               , opLetter   = oneOf ":!#$%&*+./<=>?@\\^|-~\""
               , reservedOpNames= ["+","-","*","/","\"",":", "<", "=", ">", "<=", ">="]
               , reservedNames  = ["sum", "difference", "minus", "product", "quotient", "remainder",   -- -----------
                                   "modulo", "int", "round", "sqrt", "power", "exp", "log10", "ln",    -- Expresiones
                                   "sin", "radsin", "cos", "radcos", "arctan", "radarctan", "random",  -- Aritmeticas
                                   "rerandom",                                                         -- -----------
                                   "lessp", "less?", "greaterp", "greater?", "lessequalp",             -- Expresiones
                                   "lessequal?", "greaterequalp", "greaterequal?", "equalp",           -- Booleanas
                                   "equal?", "notequalp", "notequal?", "and", "or", "not",
                                   "skip", "make", "to", "end", "call", "if", "ifelse", "for",
                                   "localmake", "stop", "repeat", "while", "do.while", "until",
                                   "do.until",
                                   "canvasSize", "forward", "fd", "backward", "bk", "left", "lt",      -- Graficos
                                   "right", "rt", "setx", "sety", "setxy", "setheading", "home",
                                   "setpc", "setpencolor", "pendown", "penup", "pd", "pu",
                                   "black", "blue", "green", "cyan", "red", "magenta", "yellow",       -- Colores
                                   "white", "brown", "tan", "forest", "aqua", "salmon", "purple",
                                   "orange", "grey"] 
              , caseSensitive  = False
               }


  logoTokens = makeTokenParser logoStyle 

  identifier' = identifier logoTokens
  reserved' = reserved logoTokens
  reservedOp' = reservedOp logoTokens
  operator' = operator logoTokens
  parens' = parens logoTokens
  brackets' = brackets logoTokens
  intOrDouble = naturalOrFloat logoTokens
  integer' = integer logoTokens
  whiteSpace' = whiteSpace logoTokens

  -- ==================================
  --    Parsers de proposito general
  -- ==================================

  -- Parser para variables

  parseVar :: Parser Variable
  parseVar = do xs <- identifier'
                if (head xs == ':') then return (tail xs)
                else return xs

  -- Parser para palabras
  -- Para referirnos a una palabra literlamente, debemos poner el caracter '"' antes de la misma. Por ejemplo
  -- si ponemos "hola nos estamos refiriendo a la palabra "hola". Citando el manual "A word with a preceding
  -- quotation represents the word itself".

  parseWord :: Parser String
  parseWord = do whiteSpace'
                 char '\"'
                 x <- many1 alphaNum
                 whiteSpace'
                 return x

  -- Parser para numeros
  -- Sea cual sea el numero usamos una reipresentacion en punto flotante

  parseNum :: Parser Double
  parseNum = do x <- intOrDouble
                return (either (fromIntegral) (id) x)
                <|>
                do whiteSpace'
                   char '.'
                   x <- integer'
                   whiteSpace'
                   return (decimals x)


  decimals :: Integer -> Double
  decimals n = (fromIntegral n) / n' 
                where n' = 10^(length (show n)) 

  -- Parser para nombres de procedimientos
  procName :: Parser String
  procName = do whiteSpace'
                x <- many1 alphaNum
                whiteSpace'
                return x
 

  -- ===================================
  --  Parseamos expresiones aritmeticas
  -- ===================================

  factor :: Parser NumExp
  factor = do reservedOp' "-"
              x <- term
              return (UnOp uMin x)
              <|> 
              do x <- parseNum
                 return (Const x)
                <|>
                do v <- parseVar 
                   return (Var v)
                   <|> 
                   do e <- parens' parseNumExp
                      return e

  term :: Parser NumExp
  term = do f <- factor
            do reservedOp' "*"
               f' <- term
               return (BinOp (*) f f')
               <|>
               do reservedOp' "/"
                  f' <- term
                  return (Div f f')
                  <|> return f
         <|> unaryOperations


  parseNumExp :: Parser NumExp
  parseNumExp = do t <- term
                   do reservedOp' "+"
                      t' <- parseNumExp
                      return (BinOp (+) t t')
                      <|>
                      do reservedOp' "-"
                         t' <- parseNumExp
                         return (BinOp (-) t t')
                      <|> return t

 -- Dependiendo de la palabra reservada, devolvemos la funcion correspondiente.
  unaryOperationsF :: Parser (Double -> Double)
  unaryOperationsF = do op <- reserved' "minus"
                        return uMin 
                     <|>
                     do op <- reserved' "int" 
                        return (fromIntegral . floor)
                     <|>
                     do op <- reserved' "round" 
                        return (fromIntegral . round)
                     <|>
                     do op <- reserved' "sqrt"
                        return sqrt 
                     <|>
                     do op <- reserved' "exp"
                        return exp
                     <|>
                     do op <- reserved' "ln"
                        return log
                     <|>
                     do op <- reserved' "log10"
                        return (logBase 10)
                     <|>
                     do op <- reserved' "sin"
                        return (sin . toRadian)
                     <|>
                     do op <- reserved' "radsin"
                        return sin
                     <|>
                     do op <- reserved' "cos"
                        return (cos . toRadian)
                     <|>
                     do op <- reserved' "radcos"
                        return cos
                     <|>
                     do op <- reserved' "arctan"
                        return (atan . toRadian) 
                     <|>
                     do op <- reserved' "radactan"
                        return atan


  unaryOperations :: Parser NumExp
  unaryOperations = do f <- unaryOperationsF
                       e <- term 
                       return (UnOp f e)

  -- Funciones Unarias para flotantes

  uMin :: Double -> Double
  uMin x = (- x)

  toRadian :: Double -> Double
  toRadian x = (x * pi) / 180

  -- ==========================
  --    Parser de booleanos
  -- ==========================

  boolConsts :: Parser BoolExp
  boolConsts = do reserved' "true"
                  return (C True)
                  <|>
                  do reserved' "false"
                     return (C False)
                     <|>
                     do v <- parseVar
                        return (VarB v)


  {- Los terminos booleanos son los que terminos que estan separados por &&, ||. Dichos terminos tiene operaciones de 
     comparacion e igualdad entre numeros -}
 
  {-(2)  el problema es que parsenumexp consume variables, entonces hay conflicto cuando quiere parsear las variables
    booleanas, por eso el try en infixPar -}


  boolTerm :: Parser BoolExp
  boolTerm = let prefixPar =  do  op <- boolTermF
                                  e  <- parseNumExp
                                  e' <- parseNumExp 
                                  return (BinOpN op e e')
                 infixPar =  do e  <- parseNumExp
                                op <- boolTermF'
                                e' <- parseNumExp 
                                return (BinOpN op e e')
                 boolC = do t <- boolConsts
                            return t
             in prefixPar <|> try parensOrBrackets <|> try infixPar <|> boolC  -- (1) (2)                 
                 
  parensOrBrackets :: Parser BoolExp
  parensOrBrackets = do e <- brackets' parseBoolExp
                        return e
                        <|>
                        do e <- parens' parseBoolExp
                           return e

  {- (1) el try lo pongo porque puede que el parseo consuma un parentesis, y lo que esta entre parentesis no sea
     una expresion booleana, por lo tanto salte al otro parser, y si la expresion numerica empezaba con un parentesis
     el mismo ya iba a estar consumido, por lo tanto el parser no sabria que hacer con el que cierra -}

  -- Para operadores formados por comandos

  boolTermF :: Parser (Double -> Double -> Bool)
  boolTermF = do reserved' "lessp"
                 return (<)
              <|>
              do reserved' "less?"
                 return (<)
              <|>
              do reserved' "greaterp"
                 return (>)
              <|>
              do reserved' "greater?"
                 return (>)
              <|>
              do reserved' "lessequalp"
                 return (<=)
              <|>
              do reserved' "lessequal?"
                 return (<=)
              <|>
              do reserved' "greaterequalp"
                 return (>=)
              <|>
              do reserved' "greaterequal?"
                 return (<=)
              <|>
              do reserved' "equalp"
                 return (==)
              <|>
              do reserved' "equal?"
                 return (==)
              
  -- Para operadores 
  
  boolTermF' :: Parser (Double -> Double -> Bool)
  boolTermF' = do reservedOp' "<"
                  return (<)
               <|>
               do reservedOp' "<="
                  return (<=)
               <|>
               do reservedOp' "="
                  return (==)
               <|>
               do reservedOp' ">"
                  return (>=)
               <|>    
               do reservedOp' ">"
                  return (>)

  -- Parser para expresiones

  parseBoolExp :: Parser BoolExp
  parseBoolExp = do t <- boolTerm
                    return t
                    <|> 
                    do reserved' "and"
                       t <- boolTerm
                       e <- parseBoolExp
                       return (BinOpB (&&) t e)
                       <|>
                       do reserved' "or"
                          t <- boolTerm
                          e <- parseBoolExp
                          return (BinOpB (||) t e)
                          <|>
                          do reserved' "not"
                             e <- parseBoolExp
                             return (Not e)

  
  -- ========================
  --    Parser de comandos
  -- ========================

  -- Los parsers de comandos los separamos en 3:
  -- Los que se pueden usar fuera de la definicion de funciones
  -- Los que se usan dentro de la definicion de funciones (localmake, estructuras de control con localmake)
  -- La combinacion de los dos

  -- General
  parseComm' :: Parser Comm
  parseComm' = do x <- reserved' "skip" 
                  return (Skip)
               <|>
               do reserved' "make"
                  x <- parseWord
                  y <- parseExp
                  return (Make x y) 
               <|>
               do reserved' "forward"                  -- Comandos de dibujos
                  n <- parseNumExp
                  return (Forward n)
               <|>
               do reserved' "fd"
                  n <- parseNumExp
                  return (Forward n)
               <|>
               do reserved' "backward"
                  n <- parseNumExp
                  return (Backward n)
               <|>
               do reserved' "bk"
                  n <- parseNumExp
                  return (Backward n)
               <|>
               do reserved' "left"
                  n <- parseNumExp
                  return (LeftAng n)
               <|>
               do reserved' "lt"
                  n <- parseNumExp
                  return (LeftAng n)
               <|>
               do reserved' "right"
                  n <- parseNumExp
                  return (RightAng n)
               <|>
               do reserved' "rt"
                  n <- parseNumExp
                  return (RightAng n)
               <|>
               do reserved' "setxy"
                  x <- parseNumExp
                  y <- parseNumExp
                  return (SetPos x y)
               <|>
               do reserved' "setx"
                  x <- parseNumExp
                  return (SetPosX x)
               <|>
               do reserved' "sety"
                  y <- parseNumExp
                  return (SetPosY y)
               <|>
               do reserved' "setheading"
                  a <- parseNumExp
                  return (SetHead a)                  -- Control del lapiz y fondo
               <|>
               do reserved' "seth"
                  a <- parseNumExp
                  return (SetHead a) 
               <|>
               do reserved' "pendown"
                  return (PenDown)
               <|>
               do reserved' "pd"
                  return (PenDown)
               <|>
               do reserved' "penup"
                  return (PenUp)
               <|>
               do reserved' "pu"
                  return (PenUp) 
               <|>
               do reserved' "setpencolor"
                  color <- parseColor
                  return (NewColor color)
               <|>
               do reserved' "setpc"
                  color <- parseColor
                  return (NewColor color)
               <|>
               do reserved' "home"
                  return (Home)                       -- Estructuras de control
               <|>
               parseProcs
              <|> -- Este es el parser para las llamadas a funciones
               parseCall
   
  -- Afuera de los metodos (sin declaracion de variables locales)
  parseCommOut :: Parser Comm
  parseCommOut = parseComm'
                 <|>
                 do reserved' "repeat"
                    n <- parseNumExp
                    coms <- brackets' parseComm
                    return (Repeat n coms)
                 <|> 
                 do reserved' "if"
                    pred <- parseBoolExp
                    coms <- brackets' parseComm
                    do coms' <- brackets' parseComm
                       return (Cond pred coms coms')
                       <|> return (Cond pred coms [Skip])
                 <|>
                 do reserved' "ifelse"
                    pred <- parseBoolExp
                    coms <- brackets' parseComm
                    coms' <- brackets' parseComm
                    return (Cond pred coms coms')
                 <|>
                 do reserved' "for"
                    ((v,s), e, j) <- brackets' parseForParams
                    coms <- brackets' parseComm
                    return (For (v,s) e j coms)
                 <|>
                 do reserved' "while"
                    tf <- parseBoolExp
                    coms <-  brackets' parseComm
                    return (While tf coms)
                 <|>
                 do reserved' "do.while"
                    coms <-  brackets' parseComm
                    tf <- parseBoolExp
                    return (DoWhile coms tf)
                 <|>
                 do reserved' "until"
                    tf <- parseBoolExp
                    coms <-  brackets' parseComm
                    return (Until tf coms)
                 <|>
                 do reserved' "do.until"
                    coms <-  brackets' parseComm
                    tf <- parseBoolExp
                    return (DoUntil coms tf)

  -- Adentro de los metodos
  parseCommIn' :: Parser Comm
  parseCommIn' = parseComm'
                 <|>
                 do reserved' "localmake"
                    x <- parseWord
                    y <- parseExp
                    return (LocalMake x y)
                 <|>
                 do reserved' "repeat"
                    n <- parseNumExp
                    coms <- brackets' parseCommIn
                    return (Repeat n coms)
                 <|> 
                 do reserved' "if"
                    pred <- parseBoolExp
                    coms <- brackets' parseCommIn
                    do coms' <- brackets' parseCommIn
                       return (Cond pred coms coms')
                       <|> return (Cond pred coms [Skip])
                 <|>
                 do reserved' "ifelse"
                    pred <- parseBoolExp
                    coms <- brackets' parseCommIn
                    coms' <- brackets' parseCommIn
                    return (Cond pred coms coms')
                 <|>
                 do reserved' "for"
                    ((v,s), e, j) <- brackets' parseForParams
                    coms <- brackets' parseCommIn
                    return (For (v,s) e j coms)
                 <|>
                 do reserved' "while"
                    tf <- parseBoolExp
                    coms <-  brackets' parseCommIn
                    return (While tf coms)
                 <|>
                 do reserved' "do.while"
                    coms <-  brackets' parseCommIn
                    tf <- parseBoolExp
                    return (DoWhile coms tf)
                 <|>
                 do reserved' "until"
                    tf <- parseBoolExp
                    coms <-  brackets' parseCommIn
                    return (Until tf coms)
                 <|>
                 do reserved' "do.until"
                    coms <-  brackets' parseCommIn
                    tf <- parseBoolExp
                    return (DoUntil coms tf)

  parseCommIn :: Parser [Comm]
  parseCommIn = many1 parseCommIn'
  

  -- Parser para los nombres de los procedimientos
  parseProcName :: Parser String
  parseProcName = do whiteSpace'
                     x <- many1 alphaNum
                     whiteSpace'
                     return x
  
  -- ParseCall

  parseCall :: Parser Comm  
  parseCall =  do reserved' "call"
                  nom <- parseProcName
                  do params <- many1 parseExp
                     return (Call nom params)
                     <|> return (Call nom [])
    
  -- Parser auxiliar para los parametros del for
  
  parseForParams :: Parser ((Variable, NumExp), NumExp, NumExp)
  parseForParams = do varname <- identifier'
                      start <- factor
                      end <- factor
                      do jumps <- factor
                         return ((varname, start), end, jumps)
                         <|> return ((varname, start), end, (Const 0.0)) 

  -- Ponemos 0 en los saltos porque no sabemos si es 1 o -1 
        
                      
 
  -- Parser de expresiones general
  parseExp :: Parser Exp
  parseExp = let numPar = do e <- parseNumExp
                             return (N e)
                 boolPar = do e <- parseBoolExp
                              return (B e)
                 numOrBool = do x <- lookAhead whatOp
                                case x of
                                  "+" -> numPar
                                  "-" -> numPar
                                  "*" -> numPar
                                  "/" -> numPar
                                  "<" -> boolPar
                                  "<=" -> boolPar
                                  "=" -> boolPar
                                  "=>" -> boolPar
                                  ">" -> boolPar
             in try numOrBool <|> try boolPar <|> numPar -- (1)


  whatOp :: Parser String
  whatOp = do parseVar
              do x <- reservedOp' "+"
                 return "+" 
                 <|>
                 do x <- reservedOp' "-"
                    return "-" 
                    <|>
                    do x <- reservedOp' "*"
                       return "*" 
                       <|>
                       do x <- reservedOp' "/"
                          return "/" 
                          <|>
                          do x <- reservedOp' "<"
                             return "<" 
                             <|>
                             do x <- reservedOp' "<="
                                return "<=" 
                                <|>
                                do x <- reservedOp' "="
                                   return "=" 
                                   <|>
                                   do x <- reservedOp' "=>"
                                      return "=>" 
                                      <|>
                                      do x <- reservedOp' ">"
                                         return ">" 

  {- (1) Antes de probar si es una expresion booleana o una numerica, nos tenemos que fijar primero si hay un
         operador infijo. Esto lo hacemos por el siguiente motivo:
         Supongamos que tenemos la expresion ":x + 1", si solo tuvieramos 'try boolPar <|> numPar' entonces el
         parser booleano parsearia correctamente la variable x, pero se encontraria con un "+", por lo tanto
         el parseo de la expresion booleana es correcto y devolvemos solo la variable x, el problema esta en que
         lo que queda por parsear ("+ 1") no corresponde a nada, por lo tanto, en realidad esta era una expresion
         numerica.
         ¿Por que sucede esto? Porque el parseo para variables tanto en expresiones booleanas como en numericas es
         el mismo, entonces, para distinguir que tipo de expresion es, nos fijamos que operador estamos usando.
  -}

  -- Parser de colores

  parseColor :: Parser (Either Colors NumExp)
  parseColor = let parseThreeCol = do r <- integer'
                                      g <- integer'
                                      b <- integer'
                                      return (fromInteger r, fromInteger g, fromInteger b)

               in do reserved' "black"
                     return (Left Black)
                  <|>
                  do reserved' "blue"
                     return (Left Blue)
                  <|> 
                  do reserved' "green"
                     return (Left Green)
                  <|> 
                  do reserved' "cyan"
                     return (Left Cyan)
                  <|> 
                  do reserved' "red"
                     return (Left Red)
                  <|> 
                  do reserved' "magenta"
                     return (Left Magenta)
                  <|> 
                  do reserved' "yellow"
                     return (Left Yellow)
                  <|> 
                  do reserved' "white"
                     return (Left White)
                  <|> 
                  do reserved' "brown"
                     return (Left Brown)
                  <|> 
                  do reserved' "tan"
                     return (Left Tan)
                  <|> 
                  do reserved' "forest"
                     return (Left Forest)
                  <|> 
                  do reserved' "aqua"
                     return (Left Aqua)
                  <|>      
                  do reserved' "salmon"
                     return (Left Salmon)
                  <|>      
                  do reserved' "purple"
                     return (Left Purple)
                  <|>      
                  do reserved' "orange"
                     return (Left Orange)
                  <|>      
                  do reserved' "grey"
                     return (Left Grey)
                  <|> 
                  do (r,g,b) <- brackets' parseThreeCol
                     return (Left $ Custom (rgb r g b)) 
                  <|> 
                  do x <- parseNumExp
                     return (Right x)



  -- Parser para procedimientos

                     
  parseProcs :: Parser Comm
  parseProcs = let parseArg = do x <- parseVar
                                 return x
                   parseOpArg = do x <- parseVar
                                   y <- parseExp
                                   return (x, y)
               in do reserved' "to"
                     name <- procName 
                     args <- many parseArg
                     opargs <- many $ brackets' parseOpArg
                     coms <- parseCommIn 
                     reserved' "end"
                     return (To name args opargs coms)

  parseComm :: Parser [Comm]
  parseComm = many1 parseCommOut

  -- Parser para los documentos lgo

  parseLogoDoc :: Parser [Comm]
  parseLogoDoc = let parseSize = do reserved' "canvassize"
                                    m <- integer'
                                    n <- integer'
                                    return (CanvasSize (fromInteger m) (fromInteger n))
                     parseBG = do reserved' "backcolor"
                                  rgb <- parseColor
                                  return (BackColor rgb)
                 in do (CanvasSize n m) <- parseSize
                       background <- parseBG
                       coms <- parseComm
                       return (((CanvasSize n m) : (background : (PenUp : ( Home : (PenDown : (coms )))))))

