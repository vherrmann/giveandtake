import { useEffect, useState } from "react";
import { Api, WithUUIDUserPublic } from "../api";
import {
  CardHeader,
  Divider,
  IconButton,
  List,
  Typography,
} from "@mui/material";
import PersonRemoveIcon from "@mui/icons-material/PersonRemove";
import { StandardCard } from "./StandardCard";
import { ListUserItem } from "./ListUserItem";
import { handleApiErr } from "../utils";

export const FriendsWidget = ({ userId }: { userId: string }) => {
  const [friends, setFriends] = useState<WithUUIDUserPublic[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  const api = Api();
  const fetchFriends = async () => {
    try {
      setLoading(true);
      const response = await api.apiFriendsGet();
      const friends = response.data;
      setFriends(friends);
    } catch (e) {
      setError("Failed to fetch friends: " + handleApiErr(e));
    }
    setLoading(false);
  };

  useEffect(() => {
    fetchFriends();
  }, []);

  const handleDelete =
    (friendId: string) => async (_event: React.MouseEvent<HTMLElement>) => {
      if (!friendId) {
        return;
      }
      try {
        await api.apiFriendsFriendIdDelete(friendId);
        setFriends(friends.filter(({ key }) => key !== friendId));
      } catch (e) {
        // FIXME: more detailed information
        setError("Failed to remove friend" + handleApiErr(e));
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

      <Divider />
      {friends.length === 0 ? (
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
    </StandardCard>
  );
};
