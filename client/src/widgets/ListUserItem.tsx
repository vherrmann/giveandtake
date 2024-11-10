import {
  ListItem,
  ListItemAvatar,
  ListItemProps,
  ListItemText,
} from "@mui/material";
import { AvatarWidget } from "./AvatarWidget";
import { LinkWidget } from "./LinkWidget";
import { UserPublic } from "../api";

export const ListUserItem = ({
  userId,
  userPublic,
  ...props
}: ListItemProps & {
  userId: string;
  // if userPublic is undefined, AvatarWidget fetches it
  // if userPublic is null, the parent will(/or can) fetch it
  userPublic?: UserPublic | null;
}) => {
  return (
    <ListItem key={userId} {...props}>
      <ListItemAvatar>
        <AvatarWidget userId={userId} userPublic={userPublic} />
      </ListItemAvatar>
      <LinkWidget to={"/user/" + userId}>
        <ListItemText primary={userPublic?.name} sx={{ fontWeight: "bold" }} />
      </LinkWidget>
    </ListItem>
  );
};
