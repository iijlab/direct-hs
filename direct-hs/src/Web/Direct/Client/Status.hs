module Web.Direct.Client.Status where

import qualified Control.Concurrent.STM as S

data Status = Active | Inactive deriving Eq
type StatusVar = S.TVar Status

isActiveSTM :: StatusVar -> S.STM Bool
isActiveSTM tvar = (== Active) <$> S.readTVar tvar

inactivate :: StatusVar -> IO ()
inactivate tvar = S.atomically $ S.writeTVar tvar Inactive

