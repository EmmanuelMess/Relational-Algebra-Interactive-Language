module StateMonad where

import           GeneralTypes
import           Errors

import qualified Data.Map            as Map

import           Control.Monad.Except

class Monad m => MonadState m where
  -- Busca el valor de una variable
  lookfor :: Variable -> ExceptT RuntimeError m Relation
  -- Cambia el valor de una variable
  update :: Variable -> Relation -> ExceptT RuntimeError m ()

-- Entornos
type Env = Map.Map Variable Relation

-- Entorno inicial
initEnv :: [(Variable, Relation)] -> Env
initEnv = Map.fromList


-- Mónada estado
newtype State a = State { runState :: Env -> (a, Env) }

instance Monad State where
  return x = State (\s -> (x, s))
  m >>= f  = State (\s ->
                    let (v, s') = runState m s
                    in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = ExceptT (State (\s ->
                let x = Map.lookup v s
                in (case x of
                      Nothing  -> (Left (UndefinedVariable v), s)
                      Just set -> (Right set, s))))
  update v i = ExceptT (State (\s -> (Right (), Map.insert v i s)))