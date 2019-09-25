{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.InternalSpec
    ( spec
    )
where

import           Control.Arrow                            ( first )
import           Control.Monad                            ( replicateM )
import           Control.Monad.State.Strict               ( State
                                                          , evalState
                                                          , get
                                                          , put
                                                          )
import           Data.Function                            ( on )
import           Data.IORef                               ( newIORef )
import           Data.List                                ( deleteBy
                                                          , partition
                                                          )
import qualified Data.Text                     as T
import           Data.Word                                ( Word64 )
import qualified Network.MessagePack.RPC.Client.Internal
                                               as RPC
import           Test.Hspec

import           Web.Direct
import           Web.Direct.Internal


spec :: Spec
spec = do
    describe "onAddTalkersTestable" $ do
        context
                "when client has the talk room with the same ID with the updated talk room's"
            $ do
                  context
                          "when SOME of the users in the updated talk room are new to the client"
                      $ it
                            "the talk room gets new users and invalidate the cache of acquaintances."
                      $ do
                            let
                                (existingRooms, newRoom, me, newUser, acquaintances)
                                    = evalIdGen $ do
                                        (me', newUser', acquaintances') <-
                                            genTestUsers
                                        roomIdToUpdate <- getNewId
                                        let newRoom' = mkTestRoom
                                                (me' : newUser' : acquaintances'
                                                )
                                                roomIdToUpdate
                                            roomToUpdate = mkTestRoom
                                                (me' : acquaintances')
                                                roomIdToUpdate

                                        otherRoom1 <-
                                            mkTestRoom
                                                    [me', head acquaintances']
                                                <$> getNewId
                                        otherRoom2 <-
                                            mkTestRoom
                                                    [me', last acquaintances']
                                                <$> getNewId

                                        return
                                            ( [ otherRoom1
                                              , roomToUpdate
                                              , otherRoom2
                                              ]
                                            , newRoom'
                                            , me'
                                            , newUser'
                                            , acquaintances'
                                            )

                            client <- newTestClient me
                                                    acquaintances
                                                    existingRooms

                            (talkRoomBeforeUpdated, otherRoomsBeforeUpdated) <-
                                getSameRoomWithOthers client newRoom

                            onAddTalkers client testDomainId newRoom

                            (updatedTalkRoom, otherRoomsAfterUpdated) <-
                                getSameRoomWithOthers client newRoom
                            talkUserIds updatedTalkRoom
                                `shouldMatchList` (  talkUserIds
                                                          talkRoomBeforeUpdated
                                                  ++ [userId newUser]
                                                  )
                            otherRoomsAfterUpdated
                                `shouldBe` otherRoomsBeforeUpdated

                            hasAcquaintancesCached client `shouldReturn` False

                  context
                          "when NONE of the users in the updated talk room are new to the client"
                      $ do
                            it
                                    "the talk room gets new users but doesn't invalidate the cache of acquaintances."
                                $ do
                                      let
                                          (existingRooms, newRoom, me, newUser, acquaintances)
                                              = evalIdGen $ do
                                                  (me', newUser', acquaintances') <-
                                                      genTestUsers
                                                  roomIdToUpdate <- getNewId
                                                  let newRoom' = mkTestRoom
                                                          ( me'
                                                          : newUser'
                                                          : acquaintances'
                                                          )
                                                          roomIdToUpdate
                                                      roomToUpdate = mkTestRoom
                                                          (me' : acquaintances')
                                                          roomIdToUpdate

                                                  otherRoom1 <-
                                                      mkTestRoom
                                                              [ me'
                                                              , head
                                                                  acquaintances'
                                                              ]
                                                          <$> getNewId
                                                  otherRoom2 <-
                                                      mkTestRoom
                                                              [ me'
                                                              , last
                                                                  acquaintances'
                                                              ]
                                                          <$> getNewId

                                                  return
                                                      ( [ otherRoom1
                                                        , roomToUpdate
                                                        , otherRoom2
                                                        ]
                                                      , newRoom'
                                                      , me'
                                                      , newUser'
                                                      , newUser'
                                                          : acquaintances'
                                                      )

                                      client <- newTestClient me
                                                              acquaintances
                                                              existingRooms

                                      (talkRoomBeforeUpdated, otherRoomsBeforeUpdated) <-
                                          getSameRoomWithOthers client newRoom

                                      onAddTalkers client testDomainId newRoom

                                      (updatedTalkRoom, otherRoomsAfterUpdated) <-
                                          getSameRoomWithOthers client newRoom
                                      talkUserIds updatedTalkRoom
                                          `shouldMatchList` (  talkUserIds
                                                                    talkRoomBeforeUpdated
                                                            ++ [ userId
                                                                     newUser
                                                               ]
                                                            )
                                      otherRoomsAfterUpdated
                                          `shouldBe` otherRoomsBeforeUpdated

                                      hasAcquaintancesCached client
                                          `shouldReturn` True

                            context
                                    "and the updated talk room has already been updated with new users"
                                $ it
                                      "the talk room gets no new users and doesn't invalidate the cache of acquaintances."
                                $ do
                                      let
                                          (existingRooms, newRoom, me, acquaintances)
                                              = evalIdGen $ do
                                                  (me', newUser', acquaintances') <-
                                                      genTestUsers
                                                  roomIdToUpdate <- getNewId
                                                  let
                                                      newRoom' = mkTestRoom
                                                          ( me'
                                                          : newUser'
                                                          : acquaintances'
                                                          )
                                                          roomIdToUpdate

                                                  otherRoom1 <-
                                                      mkTestRoom
                                                              [ me'
                                                              , head
                                                                  acquaintances'
                                                              ]
                                                          <$> getNewId
                                                  otherRoom2 <-
                                                      mkTestRoom
                                                              [ me'
                                                              , last
                                                                  acquaintances'
                                                              ]
                                                          <$> getNewId

                                                  return
                                                      ( [ otherRoom1
                                                        , newRoom'
                                                        , otherRoom2
                                                        ]
                                                      , newRoom'
                                                      , me'
                                                      , newUser'
                                                          : acquaintances'
                                                      )

                                      client <- newTestClient me
                                                              acquaintances
                                                              existingRooms

                                      roomsBeforeUpdated <- getTalkRooms client

                                      onAddTalkers client testDomainId newRoom

                                      roomsAfterUpdated <- getTalkRooms client

                                      roomsAfterUpdated
                                          `shouldMatchList` roomsBeforeUpdated

                                      hasAcquaintancesCached client
                                          `shouldReturn` True

        context
                "when client has NO talk room with the same ID with the updated talk room's"
            $ do
                  context
                          "when SOME of the users in the updated talk room are new to the client"
                      $ it
                            "the client gets the new room and invalidate the cache of acquaintances."
                      $ do
                            let
                                (existingRooms, newRoom, me, acquaintances) =
                                    evalIdGen $ do
                                        (me', newUser', acquaintances') <-
                                            genTestUsers
                                        newRoom' <-
                                            mkTestRoom
                                                    ( me'
                                                    : newUser'
                                                    : acquaintances'
                                                    )
                                                <$> getNewId
                                        otherRoom1 <-
                                            mkTestRoom
                                                    [me', head acquaintances']
                                                <$> getNewId
                                        otherRoom2 <-
                                            mkTestRoom
                                                    [me', last acquaintances']
                                                <$> getNewId

                                        return
                                            ( [otherRoom1, otherRoom2]
                                            , newRoom'
                                            , me'
                                            , acquaintances'
                                            )

                            client <- newTestClient me
                                                    acquaintances
                                                    existingRooms

                            talkRoomsBeforeUpdated <- getTalkRooms client

                            onAddTalkers client testDomainId newRoom

                            talkRoomsAfterUpdated <- getTalkRooms client
                            talkRoomsAfterUpdated
                                `shouldMatchList` newRoom
                                :                 talkRoomsBeforeUpdated

                            hasAcquaintancesCached client `shouldReturn` False

                  context
                          "when NONE of the users in the updated talk room are new to the client"
                      $ it
                            "the client gets the new room but doesn't invalidate the cache of acquaintances."
                      $ do
                            let
                                (existingRooms, newRoom, me, acquaintances) =
                                    evalIdGen $ do
                                        (me', newUser', acquaintances') <-
                                            genTestUsers
                                        newRoom' <-
                                            mkTestRoom
                                                    ( me'
                                                    : newUser'
                                                    : acquaintances'
                                                    )
                                                <$> getNewId
                                        otherRoom1 <-
                                            mkTestRoom
                                                    [ me'
                                                    , newUser'
                                                    , head acquaintances'
                                                    ]
                                                <$> getNewId
                                        otherRoom2 <-
                                            mkTestRoom
                                                    [me', last acquaintances']
                                                <$> getNewId

                                        return
                                            ( [otherRoom1, otherRoom2]
                                            , newRoom'
                                            , me'
                                            , newUser' : acquaintances'
                                            )

                            client <- newTestClient me
                                                    acquaintances
                                                    existingRooms

                            talkRoomsBeforeUpdated <- getTalkRooms client

                            onAddTalkers client testDomainId newRoom

                            talkRoomsAfterUpdated <- getTalkRooms client
                            talkRoomsAfterUpdated
                                `shouldMatchList` newRoom
                                :                 talkRoomsBeforeUpdated

                            hasAcquaintancesCached client `shouldReturn` True

    describe "onDeleteTalker" $ do
        context
                "when client has the talk room with the same ID with the updated talk room's"
            $ do
                  context
                          "when some of the users in the updated talk room DON'T SHARE any room with the client"
                      $ it
                            "the client deletes the talkers from the talk room and deletes the talkers from its acquaintances."
                      $ do
                            let
                                (existingRooms, newRoom, me, userToDelete, acquaintances)
                                    = evalIdGen $ do
                                        (me', userToDelete', acquaintances') <-
                                            genTestUsers
                                        roomIdToUpdate <- getNewId
                                        let newRoom' = mkTestRoom
                                                (me' : acquaintances')
                                                roomIdToUpdate
                                            roomToUpdate = mkTestRoom
                                                ( me'
                                                : userToDelete'
                                                : acquaintances'
                                                )
                                                roomIdToUpdate

                                        otherRoom1 <-
                                            mkTestRoom
                                                    [me', head acquaintances']
                                                <$> getNewId
                                        otherRoom2 <-
                                            mkTestRoom
                                                    [me', last acquaintances']
                                                <$> getNewId

                                        return
                                            ( [ otherRoom1
                                              , roomToUpdate
                                              , otherRoom2
                                              ]
                                            , newRoom'
                                            , me'
                                            , userToDelete'
                                            , userToDelete' : acquaintances'
                                            )

                            client <- newTestClient me
                                                    acquaintances
                                                    existingRooms

                            acquaintancesBeforeUpdated <- getAcquaintances
                                client
                            otherRoomsBeforeUpdated <-
                                filter ((/= talkId newRoom) . talkId)
                                    <$> getTalkRooms client

                            let uidsAfterDeleted = talkUserIds newRoom
                            onDeleteTalker client
                                           testDomainId
                                           (talkId newRoom)
                                           uidsAfterDeleted
                                           [userId userToDelete]

                            (updatedTalkRoom, otherRoomsAfterUpdated) <-
                                getSameRoomWithOthers client newRoom

                            talkUserIds updatedTalkRoom
                                `shouldMatchList` uidsAfterDeleted
                            otherRoomsAfterUpdated
                                `shouldBe` otherRoomsBeforeUpdated

                            acquaintancesAfterUpdated <- getAcquaintances client
                            acquaintancesAfterUpdated
                                `shouldMatchList` deleteBy
                                                      ((==) `on` userId)
                                                      userToDelete
                                                      acquaintancesBeforeUpdated

                  context
                          "when one of the users in the updated talk room STILL SHARES some room with the client"
                      $ it
                            "the client deletes the talkers from the talk room but doesn't delete the talkers from its acquaintances."
                      $ do
                            let
                                (existingRooms, newRoom, me, userToDelete, acquaintances)
                                    = evalIdGen $ do
                                        (me', userToDelete', acquaintances') <-
                                            genTestUsers
                                        roomIdToUpdate <- getNewId
                                        let newRoom' = mkTestRoom
                                                (me' : acquaintances')
                                                roomIdToUpdate
                                            roomToUpdate = mkTestRoom
                                                ( me'
                                                : userToDelete'
                                                : acquaintances'
                                                )
                                                roomIdToUpdate

                                        otherRoom1 <-
                                            mkTestRoom
                                                    [ me'
                                                    , userToDelete'
                                                    , head acquaintances'
                                                    ]
                                                <$> getNewId
                                        otherRoom2 <-
                                            mkTestRoom
                                                    [me', last acquaintances']
                                                <$> getNewId

                                        return
                                            ( [ otherRoom1
                                              , roomToUpdate
                                              , otherRoom2
                                              ]
                                            , newRoom'
                                            , me'
                                            , userToDelete'
                                            , userToDelete' : acquaintances'
                                            )

                            client <- newTestClient me
                                                    acquaintances
                                                    existingRooms

                            acquaintancesBeforeUpdated <- getAcquaintances
                                client
                            otherRoomsBeforeUpdated <-
                                filter ((/= talkId newRoom) . talkId)
                                    <$> getTalkRooms client

                            let uidsAfterDeleted = talkUserIds newRoom
                            onDeleteTalker client
                                           testDomainId
                                           (talkId newRoom)
                                           uidsAfterDeleted
                                           [userId userToDelete]

                            (updatedTalkRoom, otherRoomsAfterUpdated) <-
                                getSameRoomWithOthers client newRoom

                            talkUserIds updatedTalkRoom
                                `shouldMatchList` uidsAfterDeleted
                            otherRoomsAfterUpdated
                                `shouldBe` otherRoomsBeforeUpdated

                            acquaintancesAfterUpdated <- getAcquaintances client
                            acquaintancesAfterUpdated
                                `shouldMatchList` acquaintancesBeforeUpdated

        context
                "when client has NO talk room with the same ID with the updated talk room's"
            $ it "neither the client's talk rooms nor acquaintances change."
            $ do
                  let
                      (existingRooms, newRoom, me, userToDelete, acquaintances)
                          = evalIdGen $ do
                              (me', userToDelete', acquaintances') <-
                                  genTestUsers
                              roomIdToUpdate <- getNewId
                              let
                                  newRoom' = mkTestRoom
                                      (me' : acquaintances')
                                      roomIdToUpdate

                              otherRoom1 <-
                                  mkTestRoom
                                          [ me'
                                          , userToDelete'
                                          , head acquaintances'
                                          ]
                                      <$> getNewId
                              otherRoom2 <-
                                  mkTestRoom [me', last acquaintances']
                                      <$> getNewId

                              return
                                  ( [otherRoom1, otherRoom2]
                                  , newRoom'
                                  , me'
                                  , userToDelete'
                                  , userToDelete' : acquaintances'
                                  )

                  client <- newTestClient me acquaintances existingRooms

                  acquaintancesBeforeUpdated <- getAcquaintances client
                  otherRoomsBeforeUpdated <-
                      filter ((/= talkId newRoom) . talkId)
                          <$> getTalkRooms client

                  let uidsAfterDeleted = talkUserIds newRoom
                  onDeleteTalker client
                                 testDomainId
                                 (talkId newRoom)
                                 uidsAfterDeleted
                                 [userId userToDelete]

                  otherRoomsAfterUpdated <-
                      filter ((/= talkId newRoom) . talkId)
                          <$> getTalkRooms client
                  otherRoomsAfterUpdated `shouldBe` otherRoomsBeforeUpdated

                  acquaintancesAfterUpdated <- getAcquaintances client
                  acquaintancesAfterUpdated
                      `shouldMatchList` acquaintancesBeforeUpdated

    describe "onDeleteTalk"
        $ context
              "when client has the talk room with the same ID with the updated talk room's"
        $ context
              "when some of the users in the updated talk room DON'T SHARE any room with the client"
        $ it
              "the client deletes the talkers from the talk room and deletes the talkers from its acquaintances."
        $ do
              let
                  (existingRooms, roomIdToDelete, me, otherAcquaintance, acquaintances)
                      = evalIdGen $ do
                          (me', otherAcquaintance', acquaintancesToDelete) <-
                              genTestUsers
                          roomToDelete' <-
                              mkTestRoom
                                      ( me'
                                      : otherAcquaintance'
                                      : acquaintancesToDelete
                                      )
                                  <$> getNewId

                          otherRoom1 <-
                              mkTestRoom [me', otherAcquaintance'] <$> getNewId
                          otherRoom2 <-
                              mkTestRoom [me', otherAcquaintance'] <$> getNewId

                          return
                              ( [otherRoom1, roomToDelete', otherRoom2]
                              , talkId roomToDelete'
                              , me'
                              , otherAcquaintance'
                              , acquaintancesToDelete ++ [otherAcquaintance']
                              )

              client             <- newTestClient me acquaintances existingRooms

              roomsBeforeUpdated <- getTalkRooms client

              onDeleteTalk client roomIdToDelete

              roomsAfterUpdated <- getTalkRooms client
              roomsAfterUpdated
                  `shouldMatchList` filter ((/= roomIdToDelete) . talkId)
                                           roomsBeforeUpdated

              acquaintancesAfterUpdated <- getAcquaintances client
              acquaintancesAfterUpdated `shouldMatchList` [otherAcquaintance]

            -- This context is covered by the case above.
            -- Because `otherAcquaintance` should be left after `onDeleteTalk` because it still shares other rooms with `me`.
            -- context "when one of the users in the updated talk room STILL SHARES some room with the client" $
                -- it "the client deletes the talkers from the talk room but doesn't delete the talkers from its acquaintances." $ do

        -- As long as I experimented, on_delete_talk notification is sent only when a client calls `leaveTalkRoom`.
        -- So this onDeleteTalk should not be called in this context.
        -- context "when client has NO talk room with the same ID with the updated talk room's"


newTestClient :: User -> [User] -> [TalkRoom] -> IO Client
newTestClient user acqs rooms = do
    ss   <- RPC.initSessionState
    none <- newIORef Nothing
    let dontCall      = const (error "Don't call backend in this test")
        testRpcClient = RPC.Client
            ss
            (RPC.Backend dontCall
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
    testDomain    = Domain testDomainId "Test Domain"

mkTestUser :: T.Text -> UserId -> User
mkTestUser role uid = User
    uid
    (role <> "#" <> tshow uid <> " (Display)")
    (role <> "#" <> tshow uid <> " (Canonical Display)")
    (role <> "#" <> tshow uid <> " (Phonetic Display)")
    (role <> "#" <> tshow uid <> " (Canonical Phonetic Display)")

mkTestRoom :: [User] -> TalkId -> TalkRoom
mkTestRoom users tid =
    TalkRoom tid (GroupTalk ("Talk Room " <> tshow tid)) $ map userId users

getSameRoomWithOthers
    :: HasCallStack => Client -> TalkRoom -> IO (TalkRoom, [TalkRoom])
getSameRoomWithOthers client room =
    first head . partition ((== talkId room) . talkId) <$> getTalkRooms client


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


tshow :: Show a => a -> T.Text
tshow = T.pack . show
