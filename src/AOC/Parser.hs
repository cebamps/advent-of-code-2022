module AOC.Parser (Parser, parseOrFail, anyLineP) where

import Data.Void
import Text.Megaparsec (Parsec, anySingle, errorBundlePretty, manyTill, parse)
import Text.Megaparsec.Char (eol)

type Parser = Parsec Void String

parseOrFail :: Parser a -> String -> String -> IO a
parseOrFail parser file input = either (fail . errorBundlePretty) return $ parse parser file input

anyLineP :: Parser String
anyLineP = manyTill anySingle eol
