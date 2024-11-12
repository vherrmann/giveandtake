import {
  CardHeader,
  Divider,
  IconButton,
  Stack,
  Typography,
} from "@mui/material";
import { Api, UserPublic } from "../api";
import { PostList } from "../widgets/PostListWidget";
import { useParams } from "react-router";
import { useCallback, useEffect, useState } from "react";
import { UserAvatarWidget } from "../widgets/UserAvatarWidget";
import MoreVertIcon from "@mui/icons-material/MoreVert";
import { formatDate, handleApiErr } from "../utils";
import PersonAddIcon from "@mui/icons-material/PersonAdd";
import PersonRemoveIcon from "@mui/icons-material/PersonRemove";
import { useAuthedState } from "../ProtectedRoute";
import { LinkWidget } from "../widgets/LinkWidget";
import { StandardCard } from "../widgets/StandardCard";

const UserWidget = ({ userId }: { userId: string }) => {
  const [userPublic, setUserPublic] = useState<UserPublic | null>(null);
  const { userId: myUserId } = useAuthedState();
  const [myFriendp, setMyFriendp] = useState<boolean | null>(null);
  const [friendReqExists, setFriendReqExists] = useState<boolean | null>(null);
  const [error, setError] = useState<string | null>(null);

  const fetchInfo = useCallback(async () => {
    try {
      const response = await Api.apiUsersIdGet({ id: userId });
      setUserPublic(response.data);
    } catch (e) {
      setError(handleApiErr(e));
    }
  }, [userId]);

  const fetchMyFriendp = useCallback(async () => {
    try {
      const response = await Api.apiFriendsGet();
      const friends = response.data;
      setMyFriendp(friends.some(({ key: friendId }) => friendId === userId));
    } catch (e) {
      setError(handleApiErr(e));
    }
  }, [userId]);

  const fetchFriendReqExists = useCallback(async () => {
    try {
      const response = await Api.apiFriendsRequestGet();
      const friendRequests = response.data;
      setFriendReqExists(
        friendRequests.requestsFromYou.some(
          ({ key: friendId }) => friendId === userId,
        ),
      );
    } catch (e) {
      setError(handleApiErr(e));
    }
  }, [userId]);

  useEffect(() => {
    fetchInfo();
    fetchMyFriendp();
    fetchFriendReqExists();
  }, [userId, fetchInfo, fetchMyFriendp, fetchFriendReqExists]);

  const handleRemoveFriend = async (_event: React.MouseEvent<HTMLElement>) => {
    try {
      if (myFriendp) {
        await Api.apiFriendsFriendIdDelete({ friendId: userId });
        setMyFriendp(false);
      } else {
        await Api.apiFriendsRequestFriendIdCancelPost({ friendId: userId });
        setFriendReqExists(false);
      }
    } catch (e) {
      setError(handleApiErr(e));
    }
  };

  const handleCreateFriendRequest = async (
    _event: React.MouseEvent<HTMLElement>,
  ) => {
    try {
      await Api.apiFriendsRequestFriendIdPost({ friendId: userId });
      setFriendReqExists(true);
    } catch (e) {
      setError(handleApiErr(e));
    }
  };

  const friendAction = (() => {
    if (myFriendp === null || myUserId === userId) {
      return <></>;
    }
    if (myFriendp || friendReqExists) {
      return (
        <IconButton aria-label="remove friend" onClick={handleRemoveFriend}>
          <PersonRemoveIcon />
        </IconButton>
      );
    } else {
      return (
        <IconButton
          aria-label="create friend request"
          onClick={handleCreateFriendRequest}
        >
          <PersonAddIcon />
        </IconButton>
      );
    }
  })();

  return (
    <StandardCard>
      <CardHeader
        avatar={
          <UserAvatarWidget
            userId={userId}
            sx={{ width: 70, height: 70 }}
            userPublic={userPublic || undefined}
          />
        }
        action={
          <>
            {friendAction}
            <IconButton aria-label="settings">
              <MoreVertIcon />
            </IconButton>
          </>
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
    </StandardCard>
  );
};

export const UserRoute = () => {
  const { userId } = useParams<{ userId: string }>();
  if (!userId) {
    throw new Error("No userId provided for useParams");
  }
  const postFetcher = useCallback(async () => {
    try {
      const response = await Api.apiUsersIdPostsGet({ id: userId });
      return response.data;
    } catch (e: any) {
      if (e?.response?.status === 401) {
        return [];
      }
      throw e;
    }
  }, [userId]);

  return (
    <Stack spacing={2} alignItems="center">
      <UserWidget userId={userId} />
      <Divider flexItem />
      <PostList postsFetcher={postFetcher} />
    </Stack>
  );
};

export default UserRoute;
