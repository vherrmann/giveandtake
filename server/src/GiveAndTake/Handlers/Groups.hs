module GiveAndTake.Handlers.Groups where

import Data.Text qualified as T
import Database.Persist ((=.), (==.))
import Database.Persist qualified as P
import GiveAndTake.Api
import GiveAndTake.DB
import GiveAndTake.Handlers.Utils
import GiveAndTake.Prelude
import GiveAndTake.Types
import GiveAndTake.Utils
import Servant (type (:<|>) (..))
import Servant qualified as S

groupsHandler :: Entity User -> RServer m GroupsApi
groupsHandler userEnt =
  getGroups userEnt
    :<|> getGroupPublicH
    :<|> getGroupH userEnt
    :<|> deleteGroupH userEnt
    :<|> createGroupH userEnt
    :<|> groupsJoinReqH
    :<|> groupsRolesH
    :<|> groupsMemberH
 where
  groupsJoinReqH =
    getGroupJoinReqH userEnt
      :<|> createGroupJoinReqH userEnt
      :<|> cancelGroupJoinReqH userEnt
      :<|> acceptGroupJoinReqH userEnt
      :<|> rejectGroupJoinReqH userEnt
  groupsRolesH = changeRoleH userEnt
  groupsMemberH = addGroupMemberH userEnt :<|> removeGroupMemberH userEnt

getGroups :: Entity User -> RHandler m [WithKey' Group]
getGroups userEnt = do
  groupIds <-
    runDB (P.selectList [GroupMemberUser ==. userEnt.key] [])
      <&> fmap (.val.group)
  groupEnts <- selectByKey @Group groupIds
  pure $ fmap entityToWithKey groupEnts

removeGroupMemberH :: Entity User -> GroupId -> UserId -> RHandler m ()
removeGroupMemberH userEnt groupId userId = do
  checkIsGroupMember userEnt.key groupId
  checkIsGroupMember userId groupId
  checkIsGroupHigher userEnt.key userId groupId
  runDB $ P.deleteBy $ UniqueGroupMember groupId userId

addGroupMemberH :: Entity User -> GroupId -> UserId -> RHandler m ()
addGroupMemberH userEnt groupId userId = do
  checkIsGroupMember userEnt.key groupId
  checkIsGroupAdmin userEnt.key groupId
  whenM (isGroupMember userId groupId) $ throwError S.err409{S.errBody = "User is already a member of the group."}
  ct <- getUTCTime
  runDB $
    P.insert_
      GroupMember
        { group = groupId
        , user = userId
        , role = GroupRoleNoRole
        , createdAt = ct
        }

changeRoleH :: Entity User -> ChangeGroupRole -> RHandler m ()
changeRoleH userEnt (ChangeGroupRole{group, user = changedUser, role = newRole}) = do
  checkIsGroupMember userEnt.key group
  checkIsGroupMember changedUser group
  checkIsGroupHigher userEnt.key changedUser group
  runDB $
    P.updateWhere
      [ GroupMemberGroup ==. group
      , GroupMemberUser ==. changedUser
      ]
      [GroupMemberRole =. newRole]

rejectGroupJoinReqH :: Entity User -> GroupId -> UserId -> RHandler m ()
rejectGroupJoinReqH userEnt groupId requestingUserId = do
  checkIsGroupMember userEnt.key groupId
  checkIsGroupAdmin userEnt.key groupId
  reqExistsP <- runDB $ P.existsBy $ UniqueGroupJoinRequest requestingUserId groupId
  if reqExistsP
    then runDB $ P.deleteBy $ UniqueGroupJoinRequest requestingUserId groupId
    else throwError S.err404{S.errBody = "Group join request not found."}

acceptGroupJoinReqH :: Entity User -> GroupId -> UserId -> RHandler m ()
acceptGroupJoinReqH userEnt groupId requestingUserId = do
  group <- getByKeySE @Group groupId
  checkIsGroupMember userEnt.key groupId
  checkIsGroupAdmin userEnt.key groupId
  reqExistsP <- runDB $ P.existsBy $ UniqueGroupJoinRequest requestingUserId groupId
  if reqExistsP
    then runDB do
      P.insert_ $
        GroupMember
          { group = groupId
          , user = requestingUserId
          , role = GroupRoleNoRole
          , createdAt = group.createdAt
          }
      P.deleteBy $ UniqueGroupJoinRequest requestingUserId groupId
    else throwError S.err404{S.errBody = "Group join request not found."}

cancelGroupJoinReqH :: Entity User -> GroupId -> RHandler m ()
cancelGroupJoinReqH userEnt groupId = do
  existsP <- runDB $ P.existsBy $ UniqueGroupJoinRequest userEnt.key groupId
  if existsP
    then runDB $ P.deleteBy $ UniqueGroupJoinRequest userEnt.key groupId
    else throwError S.err404{S.errBody = "Group join request not found."}

-- FIXME: add notification
createGroupJoinReqH :: Entity User -> GroupId -> RHandler m ()
createGroupJoinReqH userEnt groupId = do
  whenM (isGroupMember userEnt.key groupId) $ throwError S.err409{S.errBody = "User is already a member of the group."}
  ct <- getUTCTime
  runDB $
    P.insert_
      GroupJoinRequest
        { from = userEnt.key
        , to = groupId
        , createdAt = ct
        }

getGroupJoinReqH :: Entity User -> RHandler m [GroupId]
getGroupJoinReqH userEnt = do
  groups <- runDB $ P.selectList [GroupJoinRequestFrom ==. userEnt.key] []
  pure $ (.val.to) <$> groups

createGroupH :: Entity User -> NewGroup -> RHandler m GroupId
createGroupH userEnt newGroup = do
  when (T.length newGroup.name > 20) $
    throwError S.err409{S.errBody = "Group name cannot be longer than 20 characters."}

  ct <- getUTCTime
  groupId <-
    runDB $ do
      uuid <-
        insertUUID $
          Group
            { name = newGroup.name
            , owner = userEnt.key
            , createdAt = ct
            }
      P.insert_ $
        GroupMember
          { group = uuid
          , user = userEnt.key
          , role = GroupRoleAdmin
          , createdAt = ct
          }
      pure uuid

  pure groupId

deleteGroupH :: Entity User -> GroupId -> RHandler m ()
deleteGroupH userEnt groupId = do
  checkIsGroupOwner userEnt.key groupId
  runDB $ P.delete groupId

-- getGroupH :: Entity User -> GroupId -> RHandler m ApiGroup
-- getGroupH userEnt groupId = do
--   group <- getByKeySE @Group groupId
--   checkIsGroupMember userEnt.key groupId
--   groupMemberEnts <- runDB $ P.selectList [GroupMemberGroup ==. groupId] []
--   runDB $ E.select $ do
--     (groupMember E.:& user) <-
--       E.from $
--         ( E.table @GroupMember
--             `E.innerJoin` E.table @User
--             `E.on` ( \(groupMember E.:& user) ->
--                       groupMember E.^. GroupMemberUser E.==. user E.^. UserId
--                    )
--         )
--     E.where_ (groupMember E.^. GroupMemberGroup E.==. E.val groupId)
--     pure (groupMember, user)

--   runDB $
--     E.select $
--       do
--         groupMember <- E.from $ E.table @GroupMember
--         E.where_ (groupMember E.^. GroupMemberGroup E.==. E.val groupId)
--         pure groupMember

--   pure todo
getGroupH = todo

getGroupPublicH :: GroupId -> RHandler m GroupPublic
getGroupPublicH groupId = do
  group <- getByKeySE @Group groupId
  pure GroupPublic{name = group.name, createdAt = group.createdAt}
