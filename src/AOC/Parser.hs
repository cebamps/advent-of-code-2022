module AOC.Parser (Parser, parseOrFail) where

import Data.Void
import Text.Megaparsec (Parsec, errorBundlePretty, parse)

type Parser = Parsec Void String

parseOrFail :: Parser a -> String -> String -> IO a
parseOrFail parser file input = either (fail . errorBundlePretty) return $ parse parser file input
