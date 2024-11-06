import {
  Avatar,
  CardContent,
  CardHeader,
  Chip,
  Divider,
  IconButton,
  List,
  Stack,
  Typography,
} from "@mui/material";
import {
  Api,
  ApiGroup,
  Group,
  GroupPublic,
  UserPublic,
  WithUUIDApiGroupMember,
  WithUUIDUserPublic,
} from "../api";
import GroupAddIcon from "@mui/icons-material/GroupAdd";
import { useParams } from "react-router";
import { useEffect, useState } from "react";
import MoreVertIcon from "@mui/icons-material/MoreVert";
import { DApi, formatDate, handleApiErr, useApiState } from "../utils";
import { LinkWidget } from "../widgets/LinkWidget";
import { StandardCard } from "../widgets/StandardCard";
import { ListUserItem } from "../widgets/ListUserItem";
import { useAuthedState } from "../ProtectedRoute";

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
  const api = Api();

  const [friendReqExists, setFriendReqExists] = useState<boolean | null>(null);

  /* const fetchFriendReqExists = async () => {
*   try {
*     const response = await api.apiFriendsRequestGet();
*     const friendRequests = response.data;
*     setFriendReqExists(
*       friendRequests.requestsFromYou.some(
*         ({ key: friendId }) => friendId === userId,
*       ),
*     );
*   } catch (e) {
*     setError(handleApiErr(e));
*   }
* };

* const handleCancelJoinRequest = async (
*   _event: React.MouseEvent<HTMLElement>,
* ) => {
*   try {
*     await api.apiFriendsFriendIdDelete(userId);
*   } catch (e) {
*     setError(handleApiErr(e));
*   }
* };

* const handleCreateJoinRequest = async (
*   _event: React.MouseEvent<HTMLElement>,
* ) => {
*   try {
*     await api.apiFriendsRequestFriendIdPost(userId);
*     setJoinReqExists(true);
*   } catch (e) {
*     setError(handleApiErr(e));
*   }
* }; */

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
          <IconButton aria-label="groupJoinRequest">
            <GroupAddIcon />
          </IconButton>
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
  const [users, setUsers, loadingGReq, errorGReq] = useApiState(
    DApi.apiGroupsRequestIdGet,
    [groupId],
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
      <Typography color="red">{errorGReq}</Typography>
      <Divider />
      <CardContent>
        <List>
          {users.map((userW) => (
            <ListUserItem
              key={userW.key}
              userId={userW.key}
              userPublic={userW.value}
              secondaryAction={null}
            />
          ))}
        </List>
      </CardContent>
    </StandardCard>
  );
};

const GroupMemberList = ({ apiGroup }: { apiGroup: ApiGroup | null }) => {
  const memberList = apiGroup?.members;

  if (!memberList || memberList.length === 0) {
    return null;
  }

  return (
    <StandardCard>
      <CardHeader
        title={
          <Typography gutterBottom variant="h5" align="center">
            Members
          </Typography>
        }
      />
      <Divider />
      <CardContent>
        <List>
          {memberList.map(({ key: userId, value: groupMember }) => (
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
                </>
              }
            />
          ))}
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
      const response = await api.apiGroupsIdGet(groupId);
      setApiGroup(response.data);
    } catch (e: any) {
      if (e?.response?.status === 401) {
        try {
          const response = await api.apiGroupsIdPublicGet(groupId);
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
        <GroupMemberList apiGroup={apiGroup} />
        {!isAdmin({ userId, apiGroup }) && (
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
