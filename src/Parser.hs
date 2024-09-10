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

---------------------------------------
--- Parser de expresiones enteras
---------------------------------------
-- Parser de constantes enteras
consExp :: Parser (Exp Int)
consExp = do nat <- natural lis
             return (Const (fromIntegral nat))

-- Parser de variables
varExp :: Parser (Exp Int)
varExp = do var <- identifier lis
            return (Var var)

-- Parser de operador ++
varInc :: Parser (Exp Int)
varInc = do var <- identifier lis
            reservedOp lis "++"
            return (VarInc var)

-- Parser de operador --
varDec :: Parser (Exp Int)
varDec = do var <- identifier lis
            reservedOp lis "--"
            return (VarDec var)

-- Parser del operador unario -
umExp :: Parser (Exp Int)
umExp = do reservedOp lis "-"
           e <- intexp5
           return (UMinus e)

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

-- Parser de asignaciones de intexp
assgExp :: Parser (Exp Int)
assgExp = do var <- identifier lis  
             reservedOp lis "="
             e <- intexp2
             return (EAssgn var e)

-- parser de precedencia de operadores
intexp0 :: Parser (Exp Int)
intexp0 = chainl1 intexp1 seqExp

intexp1 :: Parser (Exp Int)
intexp1 = try assgExp <|> intexp2

intexp2 :: Parser (Exp Int)
intexp2 = chainl1 intexp3 (minusExp <|> plusExp)

intexp3 :: Parser (Exp Int)
intexp3 = chainl1 intexp4 (timesExp <|> divExp)

intexp4 :: Parser (Exp Int)
intexp4 = varInc <|> varDec <|> umExp <|> intexp5

intexp5 :: Parser (Exp Int)
intexp5 = parens lis intexp0 <|> varExp <|> consExp
          
intexp :: Parser (Exp Int)
intexp = intexp0

---------------------------------------
--- Parser de expressiones booleanas
---------------------------------------
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
            return (\x -> (Eq x ie))

-- Parser '!='
neqBool :: Parser (Exp Int -> Exp Bool)
neqBool = do reservedOp lis "!="
             ie <- intexp
             return (\x -> (NEq x ie))

-- Parser '<'
ltBool :: Parser (Exp Int -> Exp Bool)
ltBool = do reservedOp lis "<"
            ie <- intexp
            return (\x -> (Lt x ie))

-- Parser '>'
gtBool :: Parser (Exp Int -> Exp Bool)
gtBool = do reservedOp lis ">"
            ie <- intexp
            return (\x -> (Gt x ie))

--Parser de comparación
compBool :: Parser (Exp Bool)
compBool = do ie <- intexp
              f <- gtBool <|> ltBool 
                   <|> 
                   eqBool <|> neqBool 
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
             be <- boolexp3
             return (Not be)

-- Parsers de precedencia de operadores
boolexp0 :: Parser (Exp Bool)
boolexp0 = chainl1 boolexp1 orBool

boolexp1 :: Parser (Exp Bool)
boolexp1 = chainl1 boolexp2 andBool

boolexp2 :: Parser (Exp Bool)
boolexp2 = notBool <|> boolexp3

boolexp3 :: Parser (Exp Bool)
boolexp3 = compBool <|> parens lis boolexp0 <|> atomBool

boolexp :: Parser (Exp Bool)
boolexp = boolexp0

---------------------------------------
--- Parser de comandos
---------------------------------------
-- Parser "skip"
skipComm :: Parser Comm
skipComm = do reserved lis "skip"
              return Skip

-- Parser ':='
letComm :: Parser Comm
letComm = do var <- identifier lis 
             reservedOp lis "="
             ie <- intexp
             return (Let var ie)

-- Parser ';'
seqComm :: Parser (Comm -> Comm -> Comm)
seqComm = do reservedOp lis ";"
             return Seq

-- Parser "if b {d}" y "if b {d} else {s}"
iteComm :: Parser Comm
iteComm = do reserved lis "if"
             be <- boolexp
             cm <- braces lis comm
             (do reserved lis "else"
                 cme <- braces lis comm
                 return (IfThenElse be cm cme)
                 <|>
                 return (IfThenElse be cm Skip))
         
-- Parser "repeat b until c end"
ruComm :: Parser Comm
ruComm = do reserved lis "repeat" 
            cm <- comm
            reserved lis "until"
            be <- boolexp
            reserved lis "end"
            return (RepeatUntil cm be)

-- Parser con precedencia de operadores
comm0 :: Parser Comm
comm0 = chainl1 comm1 seqComm <|> comm1

comm1 :: Parser Comm
comm1 = skipComm <|> try letComm <|> iteComm <|> ruComm

comm :: Parser Comm
comm = comm0

----------------------------------------
-- Función de parseo
----------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
