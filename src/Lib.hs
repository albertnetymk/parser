module Lib
    ( someFunc
    ) where

import Data.Scientific (floatingOrInteger)
import Control.Monad (void)
import qualified Data.Char as C (isSpace)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L
import Prelude hiding (LT, GT)

expr :: Parser Expr
expr = makeExprParser term operators
  where
    term = bool_literal
        <|> num_literal
        <|> if_
        <|> fmap Id identifier
        <|> parens expr
        <?> "expr"

    num_literal = do
      n <- lexeme L.number
      return $
          case floatingOrInteger n of
            Left x -> RealLiteral x
            Right x -> IntLiteral x

    bool_literal =
      rword "true" *> pure (BoolLiteral True)
      <|>
      rword "false" *> pure (BoolLiteral False)

    if_ = if_line <|> if_lines
      where
        if_line = do
          cond <- try $ rword "if" *> expr <* rword "then"
          then_ <- option Skip expr
          else_ <- rword "else" *> expr <* end
          return $ If cond then_ else_
        if_lines = do
          cond <- rword "if" *> expr <* nl
          then_ <- fmap Seq $ manyTill (expr <* nl) (rword "else" *> nl)
          else_ <- fmap Seq $ manyTill (expr <* nl) end
          -- else_ <- fmap Seq $ someTill (expr <* nl) end
          return $ If cond then_ else_

    assign = do
      x <- expr
      symbol "="
      e <- expr
      return $ Assign x e

    operators =
      [ []
      , [prefix "+" id, prefix "-" Neg]
      , [binary "*" $ Binary Multiply, binary "/" $ Binary Divide]
      , [binary "+" $ Binary Plus, binary "-" $ Binary Minus]
      , [binary "<" $ Binary LT, binary ">" $ Binary GT]
      , [binary "=" $ Assign]
      , [binary ";" $ append_expr]
      ]

    append_expr (Seq es) e = Seq $ es ++ [e]
    append_expr e1 e2 = Seq [e1, e2]

    binary  name f = InfixL  (f <$ symbol name)
    prefix  name f = Prefix  (f <$ symbol name)
    postfix name f = Postfix (f <$ symbol name)

end = rword "end"

tl_parser = class_parser
  <|> trait_parser
  where
    class_parser = do
      name <- rword "class" *> identifier <* nl
      methods <- many method_parser
      end <* nl
      return $ ClassDecl{name, methods}
    trait_parser = do
      name <- rword "trait" *> identifier <* nl
      methods <- many method_parser
      end <* nl
      return $ TraitDecl{name, methods}
    method_parser = do
      name <- rword "def" *> identifier
      args <- parens $ sepBy arg_parser comma
      symbol ":"
      ty <- identifier <* nl
      es <- manyTill (expr <* nl) end
      nl
      return MethodDecl{name, args, ty, es}
    arg_parser = do
      x <- identifier <* symbol ":"
      ty <- identifier
      return (x, ty)

file_parser = some tl_parser

type Type = String
type Argument = (String, Type)

data MethodDecl = MethodDecl {
  name :: String,
  args :: [Argument],
  ty :: Type,
  es :: [Expr]
} deriving (Show)

data TL = ClassDecl {
  name :: String,
  methods :: [MethodDecl]
  }
  | TraitDecl {
  name :: String,
  methods :: [MethodDecl]
  }
  deriving (Show)


data BinOp = Plus
  | Minus
  | Multiply
  | Divide
  | LT
  | GT
  deriving (Show)

data Expr = Neg Expr
  | IntLiteral Integer
  | RealLiteral Double
  | BoolLiteral Bool
  | Id String
  | Binary BinOp Expr Expr
  | If Expr Expr Expr
  | Assign Expr Expr
  | Seq [Expr]
  | Skip
  deriving (Show)

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

    is_space c = C.isSpace c && not (c == '\n' || c == '\r')

    spaceChar = satisfy is_space <?> "white space"

sc_nl :: Parser ()
sc_nl = L.space (void spaceChar) lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

semi = symbol ";"
comma = symbol ","

rword :: String -> Parser ()
rword w = try $ string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] -- list of reserved words
rws = [ "class"
      , "trait"
      , "if"
      , "then"
      , "else"
      , "end"
      , "while"
      , "do"
      , "true"
      , "false"
      , "not"
      , "and"
      , "or"
      ]

identifier :: Parser String
identifier = lexeme . try $ p >>= check
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

nl = L.lexeme sc_nl newline

testParser :: Parser [TL]
testParser = sc *> file_parser <* eof

someFunc :: IO ()
someFunc = do
  readFile "input.txt" >>= parseTest testParser
