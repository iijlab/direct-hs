{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.InternalSpec
    ( spec
    ) where

import           Control.Arrow                           (first)
import           Control.Monad                           (replicateM)
import           Control.Monad.State.Strict              (State, evalState, get,
                                                          put)
import           Data.IORef                              (IORef, newIORef,
                                                          readIORef, writeIORef)
import           Data.List                               (partition)
import qualified Data.Text                               as T
import           Data.Word                               (Word64)
import qualified Network.MessagePack.RPC.Client.Internal as RPC
import           Test.Hspec

import           Web.Direct
import           Web.Direct.Internal


spec :: Spec
spec =
    describe "onAddTalkersTestable" $ do
        context "when client has the talk room with the same ID with the updated talk room's" $ do
            context "when SOME of the users in the updated talk room are new to the client" $
                it "the talk room gets new users and invalidate the cache of acquaintances." $ do
                    let (existingRooms, newRoom, me, newUser, acquaintances) = evalIdGen $ do
                            (me', newUser', acquaintances') <- genTestUsers
                            roomIdToUpdate <- getNewId
                            let newRoom' = mkTestRoom (me' : newUser' : acquaintances') roomIdToUpdate
                                roomToUpdate = mkTestRoom (me' : acquaintances') roomIdToUpdate

                            otherRoom1 <- mkTestRoom [me', head acquaintances'] <$> getNewId
                            otherRoom2 <- mkTestRoom [me', last acquaintances'] <$> getNewId

                            return ([otherRoom1, roomToUpdate, otherRoom2], newRoom', me', newUser', acquaintances')

                    client <- newTestClient me acquaintances existingRooms

                    (talkRoomBeforeUpdated, otherRoomsBeforeUpdated) <- getSameRoomWithOthers client newRoom

                    onAddTalkers client testDomainId newRoom

                    (updatedTalkRoom, otherRoomsAfterUpdated) <- getSameRoomWithOthers client newRoom
                    talkUserIds updatedTalkRoom
                        `shouldMatchList` (talkUserIds talkRoomBeforeUpdated ++ [userId newUser])
                    otherRoomsAfterUpdated `shouldBe` otherRoomsBeforeUpdated

                    hasAcquaintancesCached client `shouldReturn` False

            context "when NONE of the users in the updated talk room are new to the client" $
                it "the talk room gets new users but doesn't invalidate the cache of acquaintances." $ do
                    let (existingRooms, newRoom, me, newUser, acquaintances) = evalIdGen $ do
                            (me', newUser', acquaintances') <- genTestUsers
                            roomIdToUpdate <- getNewId
                            let newRoom' = mkTestRoom (me' : newUser' : acquaintances') roomIdToUpdate
                                roomToUpdate = mkTestRoom (me' : acquaintances') roomIdToUpdate

                            otherRoom1 <- mkTestRoom [me', head acquaintances'] <$> getNewId
                            otherRoom2 <- mkTestRoom [me', last acquaintances'] <$> getNewId

                            return ([otherRoom1, roomToUpdate, otherRoom2], newRoom', me', newUser', newUser' : acquaintances')

                    client <- newTestClient me acquaintances existingRooms

                    (talkRoomBeforeUpdated, otherRoomsBeforeUpdated) <- getSameRoomWithOthers client newRoom

                    onAddTalkers client testDomainId newRoom

                    (updatedTalkRoom, otherRoomsAfterUpdated) <- getSameRoomWithOthers client newRoom
                    talkUserIds updatedTalkRoom
                        `shouldMatchList` (talkUserIds talkRoomBeforeUpdated ++ [userId newUser])
                    otherRoomsAfterUpdated `shouldBe` otherRoomsBeforeUpdated

                    hasAcquaintancesCached client `shouldReturn` True

        context "when client has NO talk room with the same ID with the updated talk room's" $ do
            context "when SOME of the users in the updated talk room are new to the client" $
                it "the client gets the new room and invalidate the cache of acquaintances." $ do
                    let (existingRooms, newRoom, me, acquaintances) = evalIdGen $ do
                            (me', newUser', acquaintances') <- genTestUsers
                            newRoom' <- mkTestRoom (me' : newUser' : acquaintances') <$> getNewId
                            otherRoom1 <- mkTestRoom [me', head acquaintances'] <$> getNewId
                            otherRoom2 <- mkTestRoom [me', last acquaintances'] <$> getNewId

                            return ([otherRoom1, otherRoom2], newRoom', me', acquaintances')

                    client <- newTestClient me acquaintances existingRooms

                    talkRoomsBeforeUpdated <- getTalkRooms client

                    onAddTalkers client testDomainId newRoom

                    talkRoomsAfterUpdated <- getTalkRooms client
                    talkRoomsAfterUpdated `shouldMatchList` newRoom : talkRoomsBeforeUpdated

                    hasAcquaintancesCached client `shouldReturn` False

            context "when NONE of the users in the updated talk room are new to the client" $
                it "the client gets the new room but doesn't invalidate the cache of acquaintances." $ do
                    let (existingRooms, newRoom, me, acquaintances) = evalIdGen $ do
                            (me', newUser', acquaintances') <- genTestUsers
                            newRoom' <- mkTestRoom (me' : newUser' : acquaintances') <$> getNewId
                            otherRoom1 <- mkTestRoom [me', newUser', head acquaintances'] <$> getNewId
                            otherRoom2 <- mkTestRoom [me', last acquaintances'] <$> getNewId

                            return ([otherRoom1, otherRoom2], newRoom', me', newUser' : acquaintances')

                    client <- newTestClient me acquaintances existingRooms

                    talkRoomsBeforeUpdated <- getTalkRooms client

                    hasReinitialised <- newHasCalled

                    onAddTalkers client testDomainId newRoom

                    talkRoomsAfterUpdated <- getTalkRooms client

                    talkRoomsAfterUpdated `shouldMatchList` newRoom : talkRoomsBeforeUpdated

                    hasAcquaintancesCached client `shouldReturn` True


newTestClient :: User -> [User] -> [TalkRoom] -> IO Client
newTestClient user acqs rooms = do
    ss <- RPC.initSessionState
    none <- newIORef Nothing
    let dontCall = const (error "Don't call backend in this test")
        testRpcClient = RPC.Client
            ss
            ( RPC.Backend
                dontCall
                (fail "Don't call backend in this test")
                (return ())
            )
            dontCall
            dontCall
            none

    client <- newClient testLoginInfo testRpcClient testDomain user
    setTalkRooms client rooms
    setAcquaintances client acqs
    return client
  where
    testLoginInfo = LoginInfo "accessToken" "idfv"
    testDomain = Domain testDomainId "Test Domain"

mkTestUser :: T.Text -> UserId ->  User
mkTestUser role uid = User
    uid
    (role <> "#" <> tshow uid <> " (Display)")
    (role <> "#" <> tshow uid <> " (Canonical Display)")
    (role <> "#" <> tshow uid <> " (Phonetic Display)")
    (role <> "#" <> tshow uid <> " (Canonical Phonetic Display)")

mkTestRoom :: [User] -> TalkId -> TalkRoom
mkTestRoom users tid = TalkRoom tid (GroupTalk ("Talk Room " <> tshow tid)) $ map userId users

getSameRoomWithOthers :: HasCallStack => Client -> TalkRoom -> IO (TalkRoom, [TalkRoom])
getSameRoomWithOthers client room = first head . partition ((== talkId room) . talkId) <$> getTalkRooms client


testDomainId :: DomainId
testDomainId = 9999

type IdGen = State SomeId

type SomeId = Word64

evalIdGen :: IdGen a -> a
evalIdGen = (`evalState` 1)

getNewId :: IdGen SomeId
getNewId = do
    x <- get
    put $ x + 1
    return x

genTestUsers :: IdGen (User, User, [User])
genTestUsers =
    (,,)
        <$> (mkTestUser "me" <$> getNewId)
        <*> (mkTestUser "target user" <$> getNewId)
        <*> replicateM 2 (mkTestUser "acquaintance" <$> getNewId)


newtype HasCalled = HasCalled (IORef Bool)

newHasCalled :: IO HasCalled
newHasCalled = HasCalled <$> newIORef False

markAsCalled :: HasCalled -> IO ()
markAsCalled (HasCalled ref) = writeIORef ref True

askIfHasCalled :: HasCalled -> IO Bool
askIfHasCalled (HasCalled ref) = readIORef ref


tshow :: Show a => a -> T.Text
tshow = T.pack . show
