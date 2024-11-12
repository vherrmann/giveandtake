import {
  ListItem,
  ListItemAvatar,
  ListItemProps,
  ListItemText,
} from "@mui/material";
import { UserAvatarWidget } from "./UserAvatarWidget";
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
  userPublic?: UserPublic;
}) => {
  return (
    <ListItem key={userId} {...props}>
      <ListItemAvatar>
        <UserAvatarWidget userId={userId} userPublic={userPublic} />
      </ListItemAvatar>
      <LinkWidget to={"/user/" + userId}>
        <ListItemText primary={userPublic?.name} sx={{ fontWeight: "bold" }} />
      </LinkWidget>
    </ListItem>
  );
};
