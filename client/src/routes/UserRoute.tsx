import {
  Card,
  CardActions,
  CardHeader,
  Divider,
  IconButton,
  Stack,
  Typography,
} from "@mui/material";
import { Api, UserPublic } from "../api";
import { PostList } from "../widgets/PostListWidget";
import { useParams } from "react-router";
import { useEffect, useState } from "react";
import { AvatarWidget } from "../widgets/AvatarWidget";
import MoreVertIcon from "@mui/icons-material/MoreVert";
import { formatDate, showApiErr } from "../utils";
import PersonAddIcon from "@mui/icons-material/PersonAdd";
import PersonRemoveIcon from "@mui/icons-material/PersonRemove";
import { useAuthedState } from "../ProtectedRoute";
import { LinkWidget } from "../widgets/LinkWidget";
import { StandardCard } from "../widgets/StandardCard";

const UserWidget = ({ userId }: { userId: string }) => {
  const api = new Api();
  const [userPublic, setUserPublic] = useState<UserPublic | null>(null);
  const { userId: myUserId } = useAuthedState();
  const [myFriendp, setMyFriendp] = useState<boolean | null>(null);
  const [error, setError] = useState<string | null>(null);

  const fetchInfo = async () => {
    try {
      const response = await api.apiUsersIdGet({ id: userId });
      setUserPublic(response);
    } catch (e) {
      showApiErr(e, setError);
    }
  };

  const fetchMyFriendp = async () => {
    try {
      const friends = await api.apiFriendsGet();
      setMyFriendp(friends.some(({ uuid: friendId }) => friendId === userId));
    } catch (e) {
      showApiErr(e, setError);
    }
  };

  useEffect(() => {
    fetchInfo();
    fetchMyFriendp();
  }, [userId]);

  const handleRemoveFriend = async (_event: React.MouseEvent<HTMLElement>) => {
    try {
      await api.apiFriendsFriendIdDelete({
        friendId: userId,
      });
      setMyFriendp(false);
    } catch (e) {
      showApiErr(e, setError);
    }
  };

  // TODO:
  const handleAddFriend = async (_event: React.MouseEvent<HTMLElement>) => {
    try {
      await api.apiFriendsRequestFriendIdPost({
        friendId: userId,
      });
      setMyFriendp(true);
    } catch (e) {
      showApiErr(e, setError);
    }
  };

  const friendAction = (() => {
    if (myFriendp === null || myUserId === userId) {
      return <></>;
    }
    if (myFriendp) {
      return (
        <IconButton aria-label="remove friend" onClick={handleRemoveFriend}>
          <PersonRemoveIcon />
        </IconButton>
      );
    } else {
      return (
        <IconButton aria-label="add friend" onClick={handleAddFriend}>
          <PersonAddIcon />
        </IconButton>
      );
    }
  })();

  return (
    <StandardCard>
      <CardHeader
        avatar={
          <AvatarWidget
            userId={userId}
            sx={{ width: 70, height: 70 }}
            userPublic={userPublic}
          />
        }
        action={
          <IconButton aria-label="settings">
            <MoreVertIcon />
          </IconButton>
        }
        title={
          userPublic ? (
            <LinkWidget
              to={"/user/" + userId}
              sx={{ fontWeight: "bold", fontSize: "1.5em" }}
            >
              {userPublic.name}
            </LinkWidget>
          ) : (
            "Loading…"
          )
        }
        subheader={
          userPublic ? "Joined on " + formatDate(userPublic.createdAt) : "…"
        }
      />
      <Typography color="red">{error}</Typography>
      <CardActions disableSpacing>{friendAction}</CardActions>
    </StandardCard>
  );
};

export const UserRoute = () => {
  const { userId } = useParams<{ userId: string }>();
  if (!userId) {
    throw new Error("No userId provided for useParams");
  }
  const api = new Api();
  return (
    <Stack spacing={2} alignItems="center">
      <UserWidget userId={userId} />
      <Divider flexItem />
      <PostList
        postsFetcher={async () => await api.apiUsersIdPostsGet({ id: userId })}
      />
    </Stack>
  );
};

export default UserRoute;
