module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v s of
                  Nothing -> Left UndefVar
                  Just n  -> Right n

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)

-- Evalúa comando Skip
stepComm Skip s = Right (Skip :!: s)

-- Evalúa comando de asignación
stepComm (Let v e) s = case evalExp e s of
                          Left err         -> Left err
                          Right (n :!: s') -> Right (Skip :!: update v n s')

-- Evalúa composición secuencial
stepComm (Seq Skip c1) s = Right (c1 :!: s)
stepComm (Seq c0 c1)   s = case stepComm c0 s of
                              Left err           -> Left err
                              Right (c0' :!: s') -> Right (Seq c0' c1 :!: s')

-- Evalúa ejecución condicional
stepComm (IfThenElse b c0 c1) s = case evalExp b s of
                                      Left err             -> Left err
                                      Right (True  :!: s') -> Right (c0 :!: s')
                                      Right (False :!: s') -> Right (c1 :!: s')

-- Evalúa ciclos
stepComm r@(RepeatUntil c b) s = Right (Seq c (IfThenElse b Skip r) :!: s)

-- Evalúa operador unario
evalUnOp :: (t -> a) -> Exp t -> State -> Either Error (Pair a State)
evalUnOp op e s = case evalExp e s of
                      Left err         -> Left err
                      Right (n :!: s') -> Right (op n :!: s')

-- Evalúa operador binario
evalBinOp :: (t1 -> t2 -> a) -> Exp t1 -> Exp t2 -> State -> Either Error (Pair a State)
evalBinOp op e0 e1 s = case evalExp e0 s of
                          Left err          -> Left err
                          Right (n0 :!: s') -> case evalExp e1 s' of
                                                  Left err           -> Left err
                                                  Right (n1 :!: s'') -> Right (op n0 n1 :!: s'')

-- Evalúa división
evalDiv :: Exp Int -> Exp Int -> State -> Either Error (Pair Int State)
evalDiv e0 e1 s = case evalExp e0 s of
                      Left err          -> Left err
                      Right (n0 :!: s') -> case evalExp e1 s' of
                                              Left err           -> Left err
                                              Right (n1 :!: s'') -> if n1==0 then Left DivByZero else Right (div n0 n1 :!: s'')

-- Evalúa una expresión
evalExp :: Exp a -> State -> Either Error (Pair a State)

-- Expresiones enteras
evalExp (Const nv)    s = Right (nv :!: s)
evalExp (Var x)       s = case lookfor x s of
                              Left err -> Left err
                              Right n  -> Right (n :!: s)
evalExp (VarInc x)    s = case lookfor x s of
                              Left err -> Left err
                              Right n  -> Right ((n+1) :!: update x (n+1) s)
evalExp (VarDec x)    s = case lookfor x s of
                              Left err -> Left err
                              Right n  -> Right ((n-1) :!: update x (n-1) s)
evalExp (UMinus e0)   s = evalUnOp  negate e0    s
evalExp (Plus e0 e1)  s = evalBinOp (+)    e0 e1 s
evalExp (Minus e0 e1) s = evalBinOp (-)    e0 e1 s
evalExp (Times e0 e1) s = evalBinOp (*)    e0 e1 s
evalExp (Div e0 e1)   s = evalDiv          e0 e1 s

-- Expresiones booleanas
evalExp BTrue       s = Right (True  :!: s)
evalExp BFalse      s = Right (False :!: s)
evalExp (Lt e0 e1)  s = evalBinOp (<)  e0 e1 s
evalExp (Gt e0 e1)  s = evalBinOp (>)  e0 e1 s
evalExp (And e0 e1) s = evalBinOp (&&) e0 e1 s
evalExp (Or e0 e1)  s = evalBinOp (||) e0 e1 s
evalExp (Not e0)    s = evalUnOp  not  e0    s
evalExp (Eq e0 e1)  s = evalBinOp (==) e0 e1 s
evalExp (NEq e0 e1) s = evalBinOp (/=) e0 e1 s
