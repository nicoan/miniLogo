module Main where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, eof, parse)
import Text.ParserCombinators.Parsec.Token (whiteSpace)
import Control.Monad.Except (ExceptT, runExceptT, liftIO, throwError)
import Graphics.GD (Image, Color, drawLine, savePngFile, fillImage, newImage)
import Parser (parseLogoDoc, logoTokens)
import Evaluation (CompiledImage (..), eval)
import AST

type LogoCompilerMonad = ExceptT String IO

main :: IO ()
main = do args <- getArgs
          if (length args > 2) then putStrLn "Error. Too few arguments." >> usage
          else if (length args < 2) then putStrLn "Error. Too many arguments." >> usage
          else do res <- runExceptT $ run (args!!0) (args!!1)
                  case res of
                    Left err -> putStrLn err
                    Right _ -> return ()


parseLogoSource :: Parser a -> String -> LogoCompilerMonad a
parseLogoSource p x = either (throwError . show) return (parse (whiteSpace logoTokens >> p >>= \y -> eof >> return y) "" x)


-- Ejecuta un programa a partir de su archivo fuente
run :: String -> String -> LogoCompilerMonad ()
run ifile ofile= do input <- liftIO $ readFile ifile
                    ast <- parseLogoSource parseLogoDoc input
                    compiledImage <- either throwError return (eval ast)
                    do let size = cSize compiledImage
                       let backgroundColor = cBackgroundColor compiledImage
                       let imageDescription = cDescription compiledImage
                       makeImg imageDescription size backgroundColor ofile


makeImg :: ImageDescription -> ImageSize -> Color -> String -> LogoCompilerMonad ()
makeImg img size color ofile = do pic <- liftIO $ newImage size
                                  liftIO $ fillImage color pic
                                  drawLines img pic
                                  liftIO $ savePngFile (ofile ++ ".png") pic


usage :: IO ()
usage = do putStrLn "Usage:\n"
           putStrLn "./minilogo <infile> <outfile>"
           putStrLn "<infile>  : path to the source."
           putStrLn "<outfile> : output file (appends automatically .png extension)"


drawLines :: ImageDescription -> Image -> LogoCompilerMonad ()
drawLines [] _ = return ()
drawLines (imageLine:xs) picture = do let start = startPosition imageLine
                                      let end = endPosition imageLine
                                      let color = lineColor imageLine
                                      liftIO $ drawLine start end color picture
                                      drawLines xs picture
