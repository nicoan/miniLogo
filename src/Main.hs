module Main where

import System.Environment (getArgs)
import Parser 
import Text.ParserCombinators.Parsec (many,eof,Parser,parse)
import Text.ParserCombinators.Parsec.Token (whiteSpace)
import Graphics.GD
import AST
import Evaluation
---------------------------------------------------------

main :: IO ()
main = do args <- getArgs
          if (length args > 2) then putStrLn "Error, el numero de argumentos es muy grande." >> modoDeUso
          else if (length args < 2) then putStrLn "Error, faltan argumentos." >> modoDeUso
          else run (args!!0) (args!!1)

parseIO :: Parser a -> String -> IO (Maybe a)
parseIO p x = case parse (whiteSpace logoTokens >> p >>= \y -> eof >> return y) "" x of
                  Left e -> putStrLn (show e) >> return Nothing
                  Right r -> return (Just r)


-- Ejecuta un programa a partir de su archivo fuente
run :: [Char] -> [Char] -> IO ()
run ifile ofile= do s <- readFile ifile
                    ast <- parseIO parseLogoDoc s
                    case ast of
                        Nothing -> return ()
                        Just r -> case eval r of
                                    Left e -> putStrLn e
                                    Right (i, (_, _, _, size, _, bg)) -> do makeImg i size bg ofile
                                                                            putStrLn "Compilación realizada con exito."

printList :: (Show a) => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do putStrLn $ show x
                      printList xs

makeImg :: [ImgDesc] -> (Int, Int) -> Color -> [Char] -> IO ()
makeImg img size color ofile = do pic <- newImage size
                                  fillImage color pic
                                  makeImg' img pic
                                  --printList img
                                  savePngFile (ofile ++ ".png") pic


modoDeUso :: IO ()
modoDeUso = do putStrLn "\nModo de uso:\n"
               putStrLn "./Main <infile> <outfile>"
               putStrLn "<infile>  : Archivo que contiene el codigo de fuente de entrada."
               putStrLn "<outfile> : Nombre del archivo de salida (se agregara automaticamente la extension png)"
                                  
makeImg' :: [ImgDesc] -> Image -> IO ()
makeImg' [] _ = return ()
makeImg' ((Ip ((x,y),c):xs)) p = do drawDotGD (x,y) c p
                                    makeImg' xs p
makeImg' ((Il (x,y) (w, z) c):xs) p = do drawLine (x, y) (w, z) c p
                                         makeImg' xs p


drawDotGD ::(Int, Int) -> Color -> Image -> IO ()
drawDotGD (x, y) c i = drawLine (x, y) (x, y) c i

