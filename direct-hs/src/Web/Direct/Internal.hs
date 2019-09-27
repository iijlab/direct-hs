module Web.Direct.Internal
    (
    -- * 'Web.Direct.Client'
      clientRpcClient
    , findTalkRoom
    -- ** Exposed for testing
    , onAddTalkers
    , onDeleteTalker
    , onDeleteTalk
    , newClient
    , setTalkRooms
    , hasAcquaintancesCached
    , setAcquaintances

    -- * 'Web.Direct.Exception'
    , callRpc
    , callRpcThrow
    , convertOrThrow

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
