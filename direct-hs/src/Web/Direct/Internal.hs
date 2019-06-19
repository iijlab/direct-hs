module Web.Direct.Internal
    (
    -- * 'Web.Direct.Client'
      clientRpcClient
    , findTalkRoom
    , onAddTalkers

    -- * 'Web.Direct.Exception'
    , callRpc
    , callRpcThrow
    , convertOrThrow

    -- * 'Web.Direct.DirectRPC'
    , decodeTalkRoom
    )
where

import           Web.Direct.Client
import           Web.Direct.DirectRPC.Map
import           Web.Direct.Exception
