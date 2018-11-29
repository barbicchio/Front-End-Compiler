-- automatically generated by BNF Converter
module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexLua
import ParLua
--import SkelLua
import PrintLua
import AbsLua
import TypeCheckerLua
import TacLua
import TacppLua
import Control.Monad.Writer

import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= check

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!\n"
                          showTree v tree
                          exitSuccess


showTree :: (Show a,Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> hGetContents stdin >>= check
    "-s":fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (runFile 2 pProgram) fs



check :: String -> IO ()
check s = case pProgram (myLexer s) of
            Bad err  -> do
                        putStrLn "\n-----------------------\nParse Failed...\n-----------------------\n"
                        putStrLn err
                        exitFailure
            Ok tree -> do
              putStrLn "\n-----------------------\nParse Successful!\n-----------------------\n"
              putStrLn $ show tree
              putStrLn "\n-----------------------\n CODE \n-----------------------\n"
              putStrLn $ printTree tree
              putStrLn "\n-----------------------\n TYPE CHECK \n-----------------------\n"
              mapM_ print checkedtree
              if len==0
                then do
                 putStrLn "\n-----------------------\n THREE ADRESS CODE \n-----------------------\n"
                    --putStrLn $ code tacAttr
                 print tac
                 putStrLn $ show $ prettyPrint $ code tac
              else return ()
              --string<-unwrap typecheck tree
              
               where tac = tacGenerator tree
                     checkedtree=test tree
                     len=length checkedtree
              