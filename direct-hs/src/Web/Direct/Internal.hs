module Web.Direct.Internal
    (
    -- * 'Web.Direct.Client'
      clientRpcClient
    , findTalkRoom
    , modifyTalkRooms
    -- ** Exposed for testing
    , onAddTalkers
    , onDeleteTalker
    , onDeleteTalk
    , newClient
    , setTalkRooms
    , hasAcquaintancesCached
    , setAcquaintances
    , invalidateCachedAcquaintances

    -- * 'Web.Direct.Exception'
    , callRpc
    , callRpcThrow

    -- * 'Web.Direct.DirectRPC'
    , decodeTalkRoom

    -- * 'Web.Direct.Types' (Only for testing)
    , module Web.Direct.Types
    )
where

import           Web.Direct.Client
import           Web.Direct.DirectRPC.Map
import           Web.Direct.Exception
import           Web.Direct.Types
