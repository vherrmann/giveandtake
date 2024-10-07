import { useEffect, useState } from "react";
import { Api, WithUUIDUserPublic } from "../api";
import {
  Card,
  CardContent,
  CardHeader,
  Divider,
  IconButton,
  List,
  ListItem,
  ListItemAvatar,
  ListItemText,
  Typography,
} from "@mui/material";
import { AvatarWidget } from "./AvatarWidget";
import PersonRemoveIcon from "@mui/icons-material/PersonRemove";
import { LinkWidget } from "./LinkWidget";
import { StandardCard } from "./StandardCard";
import { ListUserItem } from "./ListUserItem";

export const FriendsWidgets = ({ userId }: { userId: string }) => {
  const [friends, setFriends] = useState<WithUUIDUserPublic[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  const api = new Api();
  const fetchFriends = async () => {
    try {
      setLoading(true);
      const friends = await api.apiFriendsGet();
      setFriends(friends);
      setLoading(false);
    } catch (err) {
      // FIXME: more detailed information
      setError("Failed to fetch posts");
      setLoading(false);
    }
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
        await api.apiFriendsFriendIdDelete({
          friendId: friendId,
        });
        setFriends(friends.filter(({ uuid }) => uuid !== friendId));
      } catch (err) {
        // FIXME: more detailed information
        setError("Failed to delete friend");
      }
    };
  return (
    <StandardCard
      sx={{
        width: {
          xs: "100vw",
          sm: "75vw",
          md: "50vw",
          lg: "33vw",
          xl: "25vw",
        },
        height: "auto",
      }}
    >
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
          {friends.map(({ uuid: friendId, value: friend }) => (
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
