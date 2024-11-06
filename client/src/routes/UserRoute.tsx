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
import { formatDate, handleApiErr } from "../utils";
import PersonAddIcon from "@mui/icons-material/PersonAdd";
import PersonRemoveIcon from "@mui/icons-material/PersonRemove";
import { useAuthedState } from "../ProtectedRoute";
import { LinkWidget } from "../widgets/LinkWidget";
import { StandardCard } from "../widgets/StandardCard";

const UserWidget = ({ userId }: { userId: string }) => {
  const api = Api();
  const [userPublic, setUserPublic] = useState<UserPublic | null>(null);
  const { userId: myUserId } = useAuthedState();
  const [myFriendp, setMyFriendp] = useState<boolean | null>(null);
  const [friendReqExists, setFriendReqExists] = useState<boolean | null>(null);
  const [error, setError] = useState<string | null>(null);

  const fetchInfo = async () => {
    try {
      const response = await api.apiUsersIdGet(userId);
      setUserPublic(response.data);
    } catch (e) {
      setError(handleApiErr(e));
    }
  };

  const fetchMyFriendp = async () => {
    try {
      const response = await api.apiFriendsGet();
      const friends = response.data;
      setMyFriendp(friends.some(({ key: friendId }) => friendId === userId));
    } catch (e) {
      setError(handleApiErr(e));
    }
  };

  const fetchFriendReqExists = async () => {
    try {
      const response = await api.apiFriendsRequestGet();
      const friendRequests = response.data;
      setFriendReqExists(
        friendRequests.requestsFromYou.some(
          ({ key: friendId }) => friendId === userId,
        ),
      );
    } catch (e) {
      setError(handleApiErr(e));
    }
  };

  useEffect(() => {
    fetchInfo();
    fetchMyFriendp();
    fetchFriendReqExists();
  }, [userId]);

  const handleRemoveFriend = async (_event: React.MouseEvent<HTMLElement>) => {
    try {
      if (myFriendp) {
        await api.apiFriendsFriendIdDelete(userId);
        setMyFriendp(false);
      } else {
        await api.apiFriendsRequestFriendIdCancelPost(userId);
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
      await api.apiFriendsRequestFriendIdPost(userId);
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
          <AvatarWidget
            userId={userId}
            sx={{ width: 70, height: 70 }}
            userPublic={userPublic}
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
  const api = Api();
  return (
    <Stack spacing={2} alignItems="center">
      <UserWidget userId={userId} />
      <Divider flexItem />
      <PostList
        updateOn={userId}
        postsFetcher={async () =>
          // FIXME: add error handling?
          {
            try {
              const response = await api.apiUsersIdPostsGet(userId);
              return response.data;
            } catch (e: any) {
              if (e?.response?.status === 401) {
                return [];
              }
              throw e;
            }
          }
        }
      />
    </Stack>
  );
};

export default UserRoute;
