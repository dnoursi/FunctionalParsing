{- The basic functionality of a hypothetical RPN calculator with infinite stack -}

module Main where

import Control.Monad.State
import Data.Char
import Data.List (intercalate)
import System.IO
import Text.Regex.Posix

{- type declarations -}

data InternalState = InternalState {
    stack :: [Double],
    memory :: Double
}

type CalcState = State InternalState

{- private functions -}

pop :: CalcState Double
pop = state $ \st -> case stack st of
	[] -> (0.0,st)
	x:xs -> (x,st { stack = xs })

push :: Double -> CalcState ()
push d = modify  $ \st -> st { stack = d : stack st }

unop :: (Double -> Double) -> CalcState ()
unop op = do
    x <- pop
    push $ op x

binop :: (Double -> Double -> Double) -> CalcState ()
binop op = do
    y <- pop
    x <- pop
    push $ op x y

{- exported calculator operations -}

kEnter :: Double -> CalcState ()
kEnter = push

kAdd, kSub, kMul, kDiv :: CalcState ()
kAdd = binop (+)
kSub = binop (-)
kMul = binop (*)
kDiv = binop (/)


kSqrt :: CalcState ()
kSqrt = unop sqrt

kSin,kCos,kTan :: CalcState ()
kSin = unop sin
kCos = unop cos
kTan = unop tan

kNeg :: CalcState ()
kNeg = unop negate

{- exported stack operations -}

kSwap :: CalcState ()
kSwap = do
    y <- pop
    x <- pop
    push y
    push x

kDup :: CalcState ()
kDup = do
    x <- pop
    push x
    push x

kClear :: CalcState ()
kClear = modify $ \s -> s { stack = [] }

{- IO -}

type IOCalcState = StateT InternalState IO

while :: Monad m => m Bool -> m ()
while action = loop where
	loop = do
		continue <- action
		when continue loop

printState :: IOCalcState ()
printState = do
	state <- get
	liftIO $ do
		putStrLn $ "s = " ++ (intercalate " " $ map show $ stack state)
		putStrLn $ "m = " ++ show (memory state)

runCommands :: [String] -> IOCalcState Bool
runCommands [] = return True
runCommands (c:cs)
	| c =~ "^-?[0-9]+([.][0-9]*)?([eE]-?[0-9]+)?$" = run $ kEnter (read c)
	| c == "+" = run kAdd
	| c == "-" = run kSub
	| c == "*" = run kMul
	| c == "/" = run kDiv
	| c == "sin" = run kSin
	| c == "cos" = run kCos
	| c == "tan" = run kTan
	| c == "sqrt" = run kSqrt
	| c == "+/-" = run kNeg
	| c == "swap" = run kSwap
	| c == "dup" = run kDup
	| c == "c" = run kClear
	| c == "p" = printState >> runCommands cs
	| c == "q" = return False
    | c == "s" = run kSto
    | c == "r" = run kRcl
	| otherwise = do
		liftIO $ do
			putStrLn $ "unrecognized command: " ++ c
			putStrLn $ "commands not executed: " ++ show cs
		return True
	where
		run cmd = shift cmd >> runCommands cs
		shift = state . runState

store = modify $ \st -> st{ stack=stack st, memory = head $ stack st}
kSto = store
kRcl = gets memory

repl :: IOCalcState ()
repl = while $ do
	liftIO $ putStr ": " >> hFlush stdout
	commands <- liftIO (fmap words getLine)
	runCommands commands

startState :: InternalState
startState = InternalState { stack = [], memory = 0.0 }

withInputBuffering :: MonadIO m => BufferMode -> m a -> m a
withInputBuffering mode action = do
    savedMode <- liftIO $ hGetBuffering stdin
    liftIO $ hSetBuffering stdin mode
    result <- action
    liftIO $ hSetBuffering stdin savedMode
    return result

main :: IO ()
main = 	void $ withInputBuffering LineBuffering $ runStateT repl startState
