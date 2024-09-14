module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple
import           Data.Maybe

-- Estados
type State = M.Map Variable Int

-- Estado vacío
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor v s = fromJust $ M.lookup v s

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
stepComm :: Comm -> State -> Pair Comm State

-- Evalúa comando Skip
stepComm Skip s = Skip :!: s

-- Evalúa comando de asignación
stepComm (Let v e) s = let n :!: s' = evalExp e s
                       in Skip :!: update v n s'

-- Evalúa composición secuencial
stepComm (Seq Skip c1) s = c1 :!: s
stepComm (Seq c0 c1)   s = let c0' :!: s' = stepComm c0 s
                           in Seq c0' c1 :!: s'

-- Evalúa ejecución condicional
stepComm (IfThenElse b c0 c1) s = case evalExp b s of
                                    True  :!: s' -> c0 :!: s'
                                    False :!: s' -> c1 :!: s'

-- Evalúa ciclos
stepComm r@(RepeatUntil c b) s = Seq c (IfThenElse b Skip r) :!: s

-- Evalúa operador unario
evalUnOp :: (t -> a) -> Exp t -> State -> Pair a State
evalUnOp op e s = let n :!: s' = evalExp e s 
                  in op n :!: s'

-- Evalúa operador binario
evalBinOp :: (t1 -> t2 -> a) -> Exp t1 -> Exp t2 -> State -> Pair a State
evalBinOp op e0 e1 s = let n0 :!: s'  = evalExp e0 s
                           n1 :!: s'' = evalExp e1 s'
                       in  op n0 n1 :!: s''

-- Evalúa una expresión
evalExp :: Exp a -> State -> Pair a State

-- Expresiones enteras
evalExp (Const nv)    s = nv :!: s
evalExp (Var x)       s = lookfor x s :!: s
evalExp (VarInc x)    s = lookfor x s + 1 :!: s
evalExp (VarDec x)    s = lookfor x s - 1 :!: s
evalExp (UMinus e0)   s = evalUnOp  negate e0 s
evalExp (Plus e0 e1)  s = evalBinOp (+)    e0 e1 s
evalExp (Minus e0 e1) s = evalBinOp (-)    e0 e1 s
evalExp (Times e0 e1) s = evalBinOp (*)    e0 e1 s
evalExp (Div e0 e1)   s = evalBinOp div    e0 e1 s
evalExp (EAssgn v e)  s = let n :!: s'  = evalExp e s 
                          in n :!: update v n s'
evalExp (ESeq e1 e2)  s = let _ :!: s'  = evalExp e1 s
                              n :!: s'' = evalExp e2 s'
                          in n :!: s''

-- Expresiones booleanas
evalExp BTrue       s = True :!: s
evalExp BFalse      s = False :!: s
evalExp (Lt e0 e1)  s = evalBinOp (<)  e0 e1 s
evalExp (Gt e0 e1)  s = evalBinOp (>)  e0 e1 s
evalExp (And e0 e1) s = evalBinOp (&&) e0 e1 s
evalExp (Or e0 e1)  s = evalBinOp (||) e0 e1 s
evalExp (Not e0)    s = evalUnOp  not  e0    s
evalExp (Eq e0 e1)  s = evalBinOp (==) e0 e1 s
evalExp (NEq e0 e1) s = evalBinOp (/=) e0 e1 s
