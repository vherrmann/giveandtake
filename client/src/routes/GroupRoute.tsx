import {
  Avatar,
  Card,
  CardActions,
  CardHeader,
  Divider,
  IconButton,
  Stack,
  Typography,
} from "@mui/material";
import { Api, Group, UserPublic } from "../api";
import { PostList } from "../widgets/PostListWidget";
import { useParams } from "react-router";
import { useEffect, useState } from "react";
import { AvatarWidget } from "../widgets/AvatarWidget";
import MoreVertIcon from "@mui/icons-material/MoreVert";
import { formatDate, handleApiErr } from "../utils";
import PersonAddIcon from "@mui/icons-material/PersonAdd";
import PersonRemoveIcon from "@mui/icons-material/PersonRemove";
import { useAuthedState } from "../ProtectedRoute";
import { LinkWidget } from "../widgets/LinkWidget";
import { StandardCard } from "../widgets/StandardCard";

const GroupWidget = ({ groupId }: { groupId: string }) => {
  const api = Api();
  const [group, setGroup] = useState<Group | null>(null);
  const [error, setError] = useState<string | null>(null);

  const fetchGroup = async () => {
    try {
      const response = await api.apiGroupsIdGet(groupId);
      setGroup(response.data);
    } catch (e) {
      setError(handleApiErr(e));
    }
  };

  useEffect(() => {
    fetchGroup();
  }, [groupId]);

  /* const handleRemoveFriend = async (_event: React.MouseEvent<HTMLElement>) => {
*   try {
*     await api.apiFriendsFriendIdDelete(userId);
*     setMyFriendp(false);
*   } catch (e) {
*     setError(handleApiErr(e));
*   }
* };

* // TODO:
* const handleAddFriend = async (_event: React.MouseEvent<HTMLElement>) => {
*   try {
*     await api.apiFriendsRequestFriendIdPost(userId);
*     setMyFriendp(true);
*   } catch (e) {
*     setError(handleApiErr(e));
*   }
* };
 */
  /* const friendAction = (() => {
   *   if (myFriendp === null || myUserId === userId) {
   *     return <></>;
   *   }
   *   if (myFriendp) {
   *     return (
   *       <IconButton aria-label="remove friend" onClick={handleRemoveFriend}>
   *         <PersonRemoveIcon />
   *       </IconButton>
   *     );
   *   } else {
   *     return (
   *       <IconButton aria-label="add friend" onClick={handleAddFriend}>
   *         <PersonAddIcon />
   *       </IconButton>
   *     );
   *   }
   * })(); */

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
          group ? (
            <LinkWidget
              to={"/group/" + groupId}
              sx={{ fontWeight: "bold", fontSize: "1.5em" }}
            >
              {group.name}
            </LinkWidget>
          ) : (
            "Loading…"
          )
        }
        subheader={group ? "Created on " + formatDate(group.createdAt) : "…"}
      />
      <Typography color="red">{error}</Typography>
      {/* <CardActions disableSpacing>{groupAction}</CardActions> */}
    </StandardCard>
  );
};

const GroupMemberList = ({ groupId }: { groupId: string }) => {
  return <List></List>;
};

export const GroupRoute = () => {
  const { groupId } = useParams<{ groupId: string }>();
  if (!groupId) {
    throw new Error("No groupId provided for useParams");
  }
  const api = Api();
  return (
    <Stack spacing={2} alignItems="center">
      <GroupWidget groupId={groupId} />
      <Divider flexItem />
      <GroupMemberList groupId={groupId} />
    </Stack>
  );
};

export default GroupRoute;