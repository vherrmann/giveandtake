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
import { DApi, ErrorWidget, useApi, useApiState } from "../utils";

export const FriendsWidget = () => {
  const [errorFr, friends, { refetch }] = useApiState(DApi.apiFriendsGet);

  const [errorDFr, deleteFriend] = useApi(DApi.apiFriendsFriendIdDelete, {
    onSuccess: refetch,
  });

  return (
    <StandardCard>
      <CardHeader
        title={
          <Typography gutterBottom variant="h5" align="center">
            Friends
          </Typography>
        }
      />

      <ErrorWidget errors={[errorFr, errorDFr]} />
      <Divider />
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
                  onClick={() => deleteFriend({ friendId })}
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
