module Web.Direct
  (
    Config(..)
  , defaultConfig
  -- * Login
  , login
  , URL
  -- * Client
  , Client
  , withClient
  , clientPersistedInfo
  , setDomains
  , getDomains
  , setTalkRooms
  , getTalkRooms
  , setMe
  , getMe
  , setUsers
  , getUsers
  -- ** Persisted information
  , PersistedInfo(..)
  , serializePersistedInfo
  , deserializePersistedInfo
  -- * Message
  -- ** Ids
  , DomainId
  , TalkId
  , UserId
  , MessageId
  -- ** Main types
  , Domain
  , domainId
  , domainName
  , TalkType(..)
  , TalkRoom
  , talkId
  , talkType
  , talkUsers
  , User
  , userId
  , displayName
  , canonicalDisplayName
  , phoneticDisplayName
  , canonicalPhoneticIisplayName
  , Message(..)
  , Aux
  , auxMessageId
  , auxTalkId
  , auxUserId
  , defaultAux
  -- * Sending
  , sendMessage
  -- * Channel
  , Channel
  , withChannel
  , recv
  , send
  -- * Terminating
  , shutdown
  -- *Exceptions
  , Exception(..)
  ) where

import           Web.Direct.Api
import           Web.Direct.Client
import           Web.Direct.Message
import           Web.Direct.PersistedInfo
import           Web.Direct.Types
