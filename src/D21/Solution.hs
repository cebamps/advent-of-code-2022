{-# LANGUAGE DeriveFunctor #-}

module D21.Solution (solve) where

import AOC.Parser
import Data.Bifunctor (Bifunctor (..))
import Data.Fix (Fix (..), foldFix, unfoldFix)
import Data.Maybe (fromJust)
import Data.Ratio (Ratio, (%))
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol, letterChar, string)

data Op = Plus | Minus | Mul | Div deriving (Show)

data ExprF a f = Lit a | BinOp Op f f deriving (Show, Functor)

type AST a = Fix (ExprF a)

type RawExpr a = ExprF a String

type RawExprs a = [(String, RawExpr a)]

type Input = RawExprs Int

instance Bifunctor ExprF where
  bimap f _ (Lit a) = Lit (f a)
  bimap _ g (BinOp op x y) = BinOp op (g x) (g y)

buildFrom :: String -> RawExprs a -> AST a
buildFrom s inp = buildWith get (get s)
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

-- Ratios save the day! The two sides of the equality don't have integer
-- factors.
type Poly a = (Ratio a, Ratio a)

polyUnknown :: Integral a => Poly a
polyUnknown = (1, 0)

polyConst :: Integral a => a -> Poly a
polyConst x = (0, x % 1)

reduce :: (Show a, Integral a) => AST (Poly a) -> Poly a
reduce = foldFix $ \case
  Lit x -> x
  BinOp op x y -> runBinOpPoly op x y

runBinOpPoly :: (Show a, Integral a) => Op -> Poly a -> Poly a -> Poly a
runBinOpPoly Plus (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)
runBinOpPoly Minus (x1, x2) (y1, y2) = (x1 - y1, x2 - y2)
runBinOpPoly Mul (0, x) (y1, y2) = (x * y1, x * y2)
runBinOpPoly Mul x y@(0, _) = runBinOpPoly Mul y x
runBinOpPoly Div (x1, x2) (0, y) = (x1 / y, x2 / y)
runBinOpPoly op l r = error $ "unsupported polyonmial operation " <> show (op, l, r)

---

solve1 :: Input -> IO ()
solve1 = print . eval . buildFrom "root"

solve2 :: Input -> IO ()
solve2 inp =
  let Just (BinOp _ ln rn) = lookup "root" inp
      leftVal = reduce . buildFrom ln $ inp'
      rightVal = reduce . buildFrom rn $ inp'
   in print $ solveEquality leftVal rightVal
  where
    inp' = flip fmap inp $ \case
      ("humn", x) -> ("humn", first (const polyUnknown) x)
      (n, x) -> (n, first polyConst x)

solveEquality :: Integral a => Poly a -> Poly a -> Ratio a
-- a x + b == c y + d
solveEquality (a, b) (c, d) = (d - b) / (a - c)

---

_showAst :: Show a => AST a -> String
_showAst = foldFix $ \case
  Lit x -> show x
  BinOp op x y -> "(" <> show op <> " " <> x <> " " <> y <> ")"

---

inputP :: Parser Input
inputP = defP `sepEndBy1` eol <* eof

defP :: Parser (String, RawExpr Int)
defP = (,) <$> nameP <* string ": " <*> rawExprP

rawExprP :: Parser (RawExpr Int)
rawExprP = litP <|> binopP
  where
    litP = Lit <$> intP
    binopP = flip BinOp <$> nameP <* sep <*> opP <* sep <*> nameP
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
  solve2 inp
