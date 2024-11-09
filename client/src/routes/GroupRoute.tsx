import {
  Avatar,
  Box,
  CardContent,
  CardHeader,
  Chip,
  Dialog,
  DialogContent,
  DialogTitle,
  Divider,
  IconButton,
  List,
  ListItem,
  ListItemButton,
  ListItemText,
  Menu,
  MenuItem,
  Stack,
  Typography,
} from "@mui/material";
import {
  Api,
  ApiGroup,
  ApiGroupMember,
  Group,
  GroupPublic,
  GroupRole,
  UserPublic,
  WithUUIDApiGroupMember,
  WithUUIDUserPublic,
} from "../api";
import GroupAddIcon from "@mui/icons-material/GroupAdd";
import GroupRemoveIcon from "@mui/icons-material/GroupRemove";
import DoneIcon from "@mui/icons-material/Done";
import ClearIcon from "@mui/icons-material/Clear";
import AddIcon from "@mui/icons-material/Add";
import { useParams } from "react-router";
import { useEffect, useState } from "react";
import MoreVertIcon from "@mui/icons-material/MoreVert";
import {
  DApi,
  ErrorWidget,
  formatDate,
  getUserRole,
  groupHierarchy,
  handleApiErr,
  lesserGroupRoles,
  useApi,
  useApiState,
  user1IsHigher,
} from "../utils";
import { LinkWidget } from "../widgets/LinkWidget";
import { StandardCard } from "../widgets/StandardCard";
import { ListUserItem } from "../widgets/ListUserItem";
import { useAuthedState } from "../ProtectedRoute";
import PopupState, {
  bindDialog,
  bindMenu,
  bindTrigger,
} from "material-ui-popup-state";
import { useConfirm } from "material-ui-confirm";

const GroupInfoWidget = ({
  groupId,
  apiGroup,
}: {
  groupId: string;
  apiGroup: ApiGroup;
}) => {
  const group = apiGroup.group;

  return (
    <StandardCard>
      <CardHeader
        avatar={
          // FIXME: add proper avatar
          <Avatar sx={{ bgcolor: "#FF6666" }}>
            {
              "?" // FIXME, use AvatarWidget
            }
          </Avatar>
        }
        action={
          <IconButton aria-label="settings">
            <MoreVertIcon />
          </IconButton>
        }
        title={
          <LinkWidget
            to={"/group/" + groupId}
            sx={{ fontWeight: "bold", fontSize: "1.5em" }}
          >
            {group.name}
          </LinkWidget>
        }
        subheader={"Created on " + formatDate(group.createdAt)}
      />
      {/* <CardActions disableSpacing>{groupAction}</CardActions> */}
    </StandardCard>
  );
};

const GroupPublicInfoWidget = ({
  groupId,
  groupPublic,
}: {
  groupId: string;
  groupPublic: GroupPublic;
}) => {
  const [groupJoinReqs, errorGReq, { refetch }] = useApiState(
    DApi.apiGroupsRequestGet,
  );
  const joinReqEx = groupJoinReqs?.find((id) => id === groupId);

  const [cancelJoinReq, _loadingRJReq, errorRJReq] = useApi(
    DApi.apiGroupsRequestIdCancelPost,
    refetch,
  );
  const [postJoinReq, _loadingPJReq, errorPJReq] = useApi(
    DApi.apiGroupsRequestIdPost,
    refetch,
  );

  return (
    <StandardCard>
      <CardHeader
        avatar={
          // FIXME: add proper avatar
          <Avatar sx={{ bgcolor: "#FF6666" }}>
            {
              "?" // FIXME, use AvatarWidget
            }
          </Avatar>
        }
        action={
          joinReqEx ? (
            <IconButton
              onClick={() => cancelJoinReq({ id: groupId })}
              aria-label="groupCancelRequest"
            >
              <GroupRemoveIcon />
            </IconButton>
          ) : (
            <IconButton
              onClick={() => postJoinReq({ id: groupId })}
              aria-label="groupPostRequest"
            >
              <GroupAddIcon />
            </IconButton>
          )
        }
        title={
          <LinkWidget
            to={"/group/" + groupId}
            sx={{ fontWeight: "bold", fontSize: "1.5em" }}
          >
            {groupPublic.name}
          </LinkWidget>
        }
        subheader={"Created on " + formatDate(groupPublic.createdAt)}
      />
      <ErrorWidget errors={[errorGReq, errorPJReq, errorRJReq]} />
    </StandardCard>
  );
};

const isAdmin = ({
  userId,
  apiGroup,
}: {
  userId: string;
  apiGroup: ApiGroup;
}) => {
  const member = apiGroup.members.find(({ key }) => key === userId);
  return member?.value.role === "GroupRoleAdmin";
};

const GroupJoinrequestsWidget = ({
  groupId,
  apiGroup,
}: {
  groupId: string;
  apiGroup: ApiGroup;
}) => {
  const [users, errorGReq, { refetch }] = useApiState(
    DApi.apiGroupsRequestIdGet,
    { id: groupId },
  );

  const [acceptJoinReq, _loadingAJReq, errorAJReq] = useApi(
    DApi.apiGroupsRequestIdUserIdAcceptPost,
    refetch,
  );
  const [rejectJoinReq, _loadingRJReq, errorRJReq] = useApi(
    DApi.apiGroupsRequestIdUserIdRejectPost,
    refetch,
  );

  if (!users || users.length === 0) {
    return null;
  }

  return (
    <StandardCard>
      <CardHeader
        title={
          <Typography gutterBottom variant="h5" align="center">
            Join Requests
          </Typography>
        }
      />
      <ErrorWidget errors={[errorGReq, errorAJReq, errorRJReq]} />
      <Divider />
      <CardContent>
        <List>
          {users.map((userW) => (
            <ListUserItem
              key={userW.key}
              userId={userW.key}
              userPublic={userW.value}
              secondaryAction={
                <Box sx={{ display: "flex", gap: 1 }}>
                  <IconButton
                    edge="end"
                    aria-label="accept-request"
                    onClick={() =>
                      acceptJoinReq({ id: groupId, userId: userW.key })
                    }
                  >
                    <DoneIcon />
                  </IconButton>
                  <IconButton
                    edge="end"
                    aria-label="reject-request"
                    onClick={() =>
                      rejectJoinReq({ id: groupId, userId: userW.key })
                    }
                  >
                    <ClearIcon />
                  </IconButton>
                </Box>
              }
            />
          ))}
        </List>
      </CardContent>
    </StandardCard>
  );
};

const GroupMemberList = ({
  groupId,
  apiGroup,
}: {
  groupId: string;
  apiGroup: ApiGroup | null;
}) => {
  const { userId: myUserId } = useAuthedState();
  const memberList = apiGroup?.members;
  const confirm = useConfirm();

  // FIXME: update members
  const [removeMember, _loadingRM, errorRM] = useApi(
    DApi.apiGroupsMemberIdRemoveUserIdDelete,
  );

  // FIXME: update members
  const [changeRole, _loadingCR, errorCR] = useApi(DApi.apiGroupsRolesPost);

  if (!memberList || memberList.length === 0) {
    return null;
  }

  const memberActions = (userId: string, groupMember: ApiGroupMember) => {
    const higher = user1IsHigher({
      userId1: myUserId,
      userId2: userId,
      apiGroup,
    });
    const myUserisOwner = apiGroup.group.owner === myUserId;
    const myRole = getUserRole(myUserId, apiGroup);

    if (!myRole) {
      throw new Error("User not found in group");
    }
    const lesserRoles = myUserisOwner
      ? (Object.keys(groupHierarchy) as GroupRole[])
      : lesserGroupRoles(myRole);

    return (
      <PopupState variant="popover" popupId="post-action-menu">
        {(popupState) => (
          <>
            <IconButton {...bindTrigger(popupState)}>
              <MoreVertIcon />
            </IconButton>
            <Menu {...bindMenu(popupState)}>
              {higher && [
                <MenuItem
                  key="remove member"
                  onClick={async () => {
                    popupState.close();
                    await confirm({
                      title: "Remove member",
                      description: `Are you sure you want to remove ${groupMember.user.name}?`,
                      confirmationText: "Remove",
                      cancellationText: "Cancel",
                    });
                    removeMember({ id: groupId, userId });
                  }}
                >
                  Remove member
                </MenuItem>,
                <PopupState
                  key="change role"
                  variant="popover"
                  popupId="change member role"
                >
                  {(chmPopupState) => {
                    const handleClose = () => {
                      // FIXME: it would be cleaner to inject this somehow into popupstate
                      popupState.close();
                      chmPopupState.close();
                    };
                    return (
                      <>
                        <MenuItem
                          onClick={() => {
                            chmPopupState.open();
                          }}
                        >
                          Change role
                        </MenuItem>
                        <Dialog
                          {...bindDialog(chmPopupState)}
                          onClose={handleClose}
                        >
                          <DialogTitle>
                            Change role of {groupMember.user.name} to
                          </DialogTitle>
                          <Divider />
                          <List>
                            {lesserRoles.map((role) => (
                              <ListItem
                                key={role}
                                disablePadding
                                disableGutters
                              >
                                <ListItemButton
                                  onClick={() => {
                                    changeRole({
                                      changeGroupRole: {
                                        user: userId,
                                        group: groupId,
                                        role,
                                      },
                                    });
                                    handleClose();
                                  }}
                                >
                                  <ListItemText primary={role} />
                                </ListItemButton>
                              </ListItem>
                            ))}
                          </List>
                        </Dialog>
                      </>
                    );
                  }}
                </PopupState>,
              ]}
            </Menu>
          </>
        )}
      </PopupState>
    );
  };

  return (
    <StandardCard>
      <CardHeader
        title={
          <Typography gutterBottom variant="h5" align="center">
            Members
          </Typography>
        }
      />
      <ErrorWidget errors={[errorRM, errorCR]} />
      <Divider />
      <CardContent>
        <List>
          {memberList.map(({ key: userId, value: groupMember }) => {
            return (
              <ListUserItem
                key={userId}
                userId={userId}
                userPublic={groupMember.user}
                secondaryAction={
                  <>
                    {userId === apiGroup.group.owner ? (
                      <Chip label="Owner" color="primary" />
                    ) : groupMember.role === "GroupRoleAdmin" ? (
                      <Chip label="Admin" color="primary" />
                    ) : null}
                    {memberActions(userId, groupMember)}
                  </>
                }
              />
            );
          })}
          {/* <ListItem key="add member">
            <ListItemButton>
              <IconButton>
                <AddIcon />
              </IconButton>
              <ListItemText
                primary={"Add member"}
                sx={{ fontWeight: "bold" }}
              />
            </ListItemButton>
          </ListItem> */}
        </List>
      </CardContent>
    </StandardCard>
  );
};

export const GroupRoute = () => {
  const { groupId } = useParams<{ groupId: string }>();
  if (!groupId) {
    throw new Error("No groupId provided for useParams");
  }
  const [apiGroup, setApiGroup] = useState<ApiGroup | null>(null);
  const [groupPublic, setGroupPublic] = useState<GroupPublic | null>(null);
  const [error, setError] = useState<string | null>(null);
  const { userId } = useAuthedState();
  const api = Api();

  const fetchGroup = async () => {
    try {
      const response = await api.apiGroupsIdGet({ id: groupId });
      setApiGroup(response.data);
    } catch (e: any) {
      if (e?.response?.status === 401) {
        try {
          const response = await api.apiGroupsIdPublicGet({ id: groupId });
          setGroupPublic(response.data);
        } catch (e) {
          setError(handleApiErr(e));
        }
      } else {
        setError(handleApiErr(e));
      }
    }
  };

  useEffect(() => {
    fetchGroup();
  }, [groupId]);

  if (apiGroup) {
    return (
      <Stack spacing={2} alignItems="center">
        <GroupInfoWidget groupId={groupId} apiGroup={apiGroup} />
        <Typography color="red">{error}</Typography>
        <Divider flexItem />
        <GroupMemberList groupId={groupId} apiGroup={apiGroup} />
        {isAdmin({ userId, apiGroup }) && (
          <GroupJoinrequestsWidget groupId={groupId} apiGroup={apiGroup} />
        )}
      </Stack>
    );
  } else if (groupPublic) {
    return (
      <Stack spacing={2} alignItems="center">
        <GroupPublicInfoWidget groupId={groupId} groupPublic={groupPublic} />
        <Typography color="red">{error}</Typography>
      </Stack>
    );
  } else {
    return (
      <>
        <Typography>Loading…</Typography>{" "}
        <Typography color="red">{error}</Typography>
      </>
    );
  }
};

export default GroupRoute;
