import { useEffect, useState } from "react";
import { Api, WithUUIDUserPublic } from "../api";
import {
  CardContent,
  CardHeader,
  Divider,
  IconButton,
  List,
  Typography,
} from "@mui/material";
import PersonRemoveIcon from "@mui/icons-material/PersonRemove";
import { StandardCard } from "./StandardCard";
import { ListUserItem } from "./ListUserItem";
import { DApi, handleApiErr, useApiState } from "../utils";

export const FriendsWidget = ({ userId }: { userId: string }) => {
  const api = Api();
  const [error, setError] = useState<string | null>(null);
  const [friends, setFriends, loadingFr, errorFr] = useApiState(
    DApi.apiFriendsGet,
    undefined,
  );

  const handleDelete =
    (friendId: string) => async (_event: React.MouseEvent<HTMLElement>) => {
      if (!friendId) {
        return;
      }
      try {
        await api.apiFriendsFriendIdDelete({ friendId: friendId });
        setFriends(friends?.filter(({ key }) => key !== friendId) ?? null);
      } catch (e) {
        setError("Failed to remove friend: " + handleApiErr(e));
      }
    };
  return (
    <StandardCard>
      <CardHeader
        title={
          <Typography gutterBottom variant="h5" align="center">
            Friends
          </Typography>
        }
      />

      <Typography color="red">
        {error}
        {errorFr}
      </Typography>
      <Divider />
      <CardContent>
        {friends === null ? (
          <Typography sx={{ margin: 1 }} align="center">
            Loading...
          </Typography>
        ) : friends.length === 0 ? (
          <Typography sx={{ margin: 1 }} align="center">
            No friends D: Add some!
          </Typography>
        ) : (
          <List>
            {friends.map(({ key: friendId, value: friend }) => (
              <ListUserItem
                userId={friendId}
                userPublic={friend}
                key={friendId}
                secondaryAction={
                  <IconButton
                    edge="end"
                    aria-label="delete"
                    onClick={handleDelete(friendId)}
                  >
                    <PersonRemoveIcon />
                  </IconButton>
                }
              />
            ))}
          </List>
        )}
      </CardContent>
    </StandardCard>
  );
};
