module Parser where

import Text.ParserCombinators.ReadP
import Data.Char


type ErrMsg = String -- error message
type VName  = String -- variable name
type FName  = String -- function name, including operators

data Expr =
-- ^ An expression is either a variable, an integer, or a function of (possibly
--   zero) arguments.
  TVar VName | TNum Integer | TFun FName [Expr]
  deriving (Eq, Ord, Show, Read)

data Assoc =
-- ^ Associativity of operators.
  ALeft | ARight | ANone
  deriving (Eq, Show, Read)

data OpTable = OpTable [(Assoc, [FName])]
-- ^ An operator table gathers operators and their associativity. Operators are
--   assumed to appear in increasing order of precedence. Operators of same
--   precedence are assumed to have the same associativity.
  deriving (Eq, Show, Read)

arithmetic :: OpTable
-- ^ Example operator table.
arithmetic =  OpTable [
  (ANone, ["<=", "<"]),
  (ALeft, ["+", "-"]),
  (ALeft, ["*"]),
  (ARight, ["**"])
  ]

--------------------------------------------------------------------------------

parseStringExpr :: OpTable -> String -> Either ErrMsg Expr
-- ^ Main parsing function.
parseStringExpr ot s = case readP_to_S parser s of
                                 []           -> Left "Parse error."
                                 [(t, "")]    -> Right t
                                 _            -> Left "Unknown error."
  where parser = do skipSpaces
                    res <- expr ot
                    eof
                    return res

expr :: OpTable -> ReadP Expr
-- ^ Parser for expressions.
expr ot = chainops (rexpr ot) ot

chainops :: ReadP Expr -> OpTable -> ReadP Expr
-- ^ See documentation.
chainops o0 (OpTable [])            = o0
chainops o0 (OpTable [(assoc,(o:os))]) = case assoc of
  ALeft  -> chainl1 o0 $ foldl (<++) (op o) (map op os)
  ARight -> chainr1 o0 $ foldl (<++) (op o) (map op os)
  ANone  -> chainx1 o0 $ foldl (<++) (op o) (map op os)
chainops o0 (OpTable (oe:oes)) =
  chainops (chainops o0 (OpTable oes)) (OpTable [oe])

chainx1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
-- ^ @chainx1@ is the non-associative analog of the @ReadP@ functions @chainr1@
--   and @chainl1@.
p `chainx1` po = (do a1 <- p
                     o <- po
                     a2 <- p
                     return (a1 `o` a2))
                 +++ p

rexpr :: OpTable -> ReadP Expr
-- ^ A restricted expr is an expr that can serve as an operand.
rexpr ot = do (token $ number)
                +++ (token $ variable)
                +++ (token $ function ot)
                +++ (token $ parens (expr ot))

op :: String -> ReadP (Expr -> Expr -> Expr)
-- ^ Given a string representation of an operator, return a parser for this
--   operator.
op o = do symbol o
          return $ \t1 t2 -> TFun o [t1,t2]

number :: ReadP Expr
-- ^ Parses a number. A single tilde prefix @~@ is used to denote a negative
--   integer.
number = (do n <- token $ natnum -- natnum stands for natural number
             return $ TNum n)
         +++
         (do string "~"
             n <- token $ natnum
             return $ TNum (-n))

name :: ReadP String
-- ^ Parses a name, i.e. an alphanumeric string starting with a letter.
name = (do c  <- letter
           cs <- many letterOrDigit
           return (c:cs))
  where
    letter = satisfy isLetter
    letterOrDigit = satisfy isLetter +++ satisfy isDigit

variable :: ReadP Expr
variable =  do n <- name
               return $ TVar n

function :: OpTable -> ReadP Expr
function ot = do cs <- token $name
                 tz <- token $parens $ exprz ot
                 return $ TFun cs tz


exprz :: OpTable -> ReadP [Expr]
-- ^ Parses zero or more exprs, separated by commas.
exprz ot =  expr ot `sepBy` symbol ","

exprs :: OpTable -> ReadP [Expr]
-- ^ Parses one or more exprs, separated by commas.
exprs ot = expr ot `sepBy1` symbol ","

-- generic parsing functions -------------------------------------------------

parens :: ReadP a -> ReadP a
parens p = do symbol "("
              res <- token $ p
              symbol ")"
              return res

token :: ReadP a -> ReadP a
token p  = do a <- p
              skipSpaces
              return a

symbol :: String -> ReadP ()
symbol s = do _ <- token $ string s
              return ()

natnum :: ReadP Integer
-- ^ Parses a natural number, i.e. a non-negative integer.
natnum = do ns <- many1 digit
            return $ read ns
              where digit = satisfy isDigit
