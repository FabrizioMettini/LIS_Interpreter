module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "++"
                        , "--"
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
-- Parser de constantes enteras
consExp :: Parser (Exp Int)
consExp = do nat <- natural lis
             return (Const (fromIntegral nat))

-- Parser de variables
varExp :: Parser (Exp Int)
varExp = do var <- identifier lis
            do reservedOp lis "++"
               return (VarInc var)
               <|>
               do reservedOp lis "--"
                  return (VarDec var)
                  <|>
                  return (Var var)

-- Parser del operador unario -
umExp :: Parser (Exp Int)
umExp = do reservedOp lis "-"
           UMinus <$> intexp2

-- Parser de operador +
plusExp :: Parser (Exp Int -> Exp Int -> Exp Int)
plusExp = do reservedOp lis "+"
             return Plus

-- Parser de operador -
minusExp :: Parser (Exp Int -> Exp Int -> Exp Int)
minusExp = do reservedOp lis "-"
              return Minus

-- Parser de operador *
timesExp :: Parser (Exp Int -> Exp Int -> Exp Int)
timesExp = do reservedOp lis "*"
              return Times

-- Parser de operador /
divExp :: Parser (Exp Int -> Exp Int -> Exp Int)
divExp = do reservedOp lis "/"
            return Div

-- parser de precedencia de operadores

intexp0 :: Parser (Exp Int)
intexp0 = chainl1 intexp1 (minusExp <|> plusExp)

intexp1 :: Parser (Exp Int)
intexp1 = chainl1 intexp2 (timesExp <|> divExp)

intexp2 :: Parser (Exp Int)
intexp2 = umExp <|> intexp3

intexp3 :: Parser (Exp Int)
intexp3 = parens lis intexp0 <|> consExp <|> varExp

intexp :: Parser (Exp Int)
intexp = intexp0

------------------------------------
--- Parser de expresiones booleanas
------------------------------------
-- Parser expresion atómica
atomBool :: Parser (Exp Bool)
atomBool = trueBool <|> falseBool
  where 
    trueBool  = do { reserved lis "true"  ; return BTrue  }
    falseBool = do { reserved lis "false" ; return BFalse }

-- Parser '=='
eqBool :: Parser (Exp Int -> Exp Bool)
eqBool = do reservedOp lis "=="
            ie <- intexp
            return (`Eq` ie)

-- Parser '!='
neqBool :: Parser (Exp Int -> Exp Bool)
neqBool = do reservedOp lis "!="
             ie <- intexp
             return (`NEq` ie)

-- Parser '<'
ltBool :: Parser (Exp Int -> Exp Bool)
ltBool = do reservedOp lis "<"
            ie <- intexp
            return (`Lt` ie)

-- Parser '>'
gtBool :: Parser (Exp Int -> Exp Bool)
gtBool = do reservedOp lis ">"
            ie <- intexp
            return (`Gt` ie)

--Parser de comparación
compBool :: Parser (Exp Bool)
compBool = do ie <- intexp
              f <- gtBool <|> ltBool <|> eqBool <|> neqBool
              return (f ie)

-- Parser '&&'
andBool :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andBool = do reservedOp lis "&&"
             return And

-- Parser '||' 
orBool :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orBool = do reservedOp lis "||"
            return Or

-- Parser '!'
notBool :: Parser (Exp Bool)
notBool = do reservedOp lis "!"
             Not <$> boolexp2

-- Parsers de precedencia de operadores
boolexp0 :: Parser (Exp Bool)
boolexp0 = chainl1 boolexp1 orBool

boolexp1 :: Parser (Exp Bool)
boolexp1 = chainl1 boolexp2 andBool

boolexp2 :: Parser (Exp Bool)
boolexp2 = notBool <|> boolexp3

boolexp3 :: Parser (Exp Bool)
boolexp3 = parens lis boolexp0 <|> atomBool <|> compBool

boolexp :: Parser (Exp Bool)
boolexp = boolexp0

-----------------------------------
--- Parser de comandos
-----------------------------------
-- Parser "skip"
skipComm :: Parser Comm
skipComm = do reserved lis "skip"
              return Skip

-- Parser ':='
letComm :: Parser Comm
letComm = do var <- identifier lis 
             reservedOp lis "="
             Let var <$> intexp

-- Parser ';'
seqComm :: Parser (Comm -> Comm -> Comm)
seqComm = do reservedOp lis ";"
             return Seq

-- Parser "if b {d}" y "if b {d} else {s}"
iteComm :: Parser Comm
iteComm = do reserved lis "if"
             be <- boolexp
             cm <- braces lis comm
             do reserved lis "else"
                cme <- braces lis comm
                return (IfThenElse be cm cme)
                <|>
                return (IfThen be cm)

-- Parser "repeat b until c"
ruComm :: Parser Comm
ruComm = do reserved lis "repeat" 
            cm <- braces lis comm
            reserved lis "until"
            RepeatUntil cm <$> boolexp

-- Parser con precedencia de operadores
comm0 :: Parser Comm
comm0 = chainl1 comm1 seqComm

comm1 :: Parser Comm
comm1 = skipComm <|> iteComm <|> ruComm <|> letComm

comm :: Parser Comm
comm = comm0

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

