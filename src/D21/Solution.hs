{-# LANGUAGE DeriveFunctor #-}

module D21.Solution (solve) where

import AOC.Parser
import Data.Fix (Fix (..), foldFix, unfoldFix)
import Data.Maybe (fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, string)

data Op = Plus | Minus | Mul | Div deriving (Show)

data ExprF a f = Lit a | BinOp Op f f deriving (Show, Functor)

type AST a = Fix (ExprF a)

type RawExpr a = ExprF a String

type Input = [(String, RawExpr Int)]

build :: Input -> AST Int
build inp = buildWith get (get "root")
  where
    get k = fromJust $ lookup k inp

buildWith :: (String -> RawExpr a) -> RawExpr a -> AST a
buildWith look = unfoldFix $ \case
  Lit x -> Lit x
  BinOp op x y -> BinOp op (look x) (look y)

eval :: Integral a => AST a -> a
eval = foldFix $ \case
  Lit x -> x
  BinOp op x y -> runBinOp op x y

runBinOp :: Integral a => Op -> a -> a -> a
runBinOp Plus = (+)
runBinOp Minus = (-)
runBinOp Mul = (*)
-- the instructions don't talk about integer division, but they don't talk
-- about floating point either
runBinOp Div = div

---

solve1 :: Input -> IO ()
solve1 = print . eval . build

---

inputP :: Parser Input
inputP = defP `sepEndBy1` eol <* eof

defP :: Parser (String, RawExpr Int)
defP = (,) <$> nameP <* string ": " <*> rawExprP

rawExprP :: Parser (RawExpr Int)
rawExprP = litP <|> binopP
  where
    litP = Lit <$> intP
    binopP = flip BinOp <$> nameP <* sep <*> opP  <* sep <*> nameP
    sep = char ' '

nameP :: Parser String
nameP = some (letterChar <|> char '_')

opP :: Parser Op
opP = choice [op <$ char c | (c, op) <- ops]
  where
    ops =
      [ ('+', Plus),
        ('-', Minus),
        ('*', Mul),
        ('/', Div)
      ]

intP :: Parser Int
intP = read <$> some digitChar

---

solve :: String -> IO ()
solve s = do
  inp <- parseOrFail inputP "input" s
  solve1 inp
