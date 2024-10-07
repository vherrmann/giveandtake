import {
  Card,
  CardHeader,
  Divider,
  IconButton,
  ListItemIcon,
  Menu,
  MenuItem,
} from "@mui/material";
import Typography from "@mui/material/Typography";
import { useAuth } from "../providers/auth";
import PopupState, { bindMenu, bindTrigger } from "material-ui-popup-state";
import LogoutIcon from "@mui/icons-material/Logout";
import { AvatarWidget } from "./AvatarWidget";
import { useAuthedState } from "../ProtectedRoute";
import { formatDate, userToUserPublic } from "../utils";
import { LinkWidget } from "./LinkWidget";

import SettingsIcon from "@mui/icons-material/Settings";
import { User } from "../api";
import { popupBarStyle } from "../style";

const UserWidget = () => {
  const { userId, user } = useAuthedState();

  return (
    <Card sx={{ width: "100%" }}>
      <CardHeader
        avatar={
          <AvatarWidget
            nolink
            userId={userId}
            userPublic={userToUserPublic(user)}
          />
        }
        title={<Typography sx={{ fontWeight: "bold" }}>{user.name}</Typography>}
        subheader={"Joined on " + formatDate(user.createdAt)}
      />
    </Card>
  );
};

export const AvatarMenu = ({
  userId,
  user,
}: {
  userId: string;
  user: User;
}) => {
  const { logout } = useAuth();
  return (
    <PopupState variant="popover" popupId="avatar-menu-popup">
      {(popupState) => (
        <>
          <IconButton aria-label="share" {...bindTrigger(popupState)}>
            <AvatarWidget
              nolink
              userId={userId}
              userPublic={{ name: user.name, createdAt: user.createdAt }}
            />
          </IconButton>
          <Menu {...bindMenu(popupState)} {...popupBarStyle}>
            <LinkWidget to={"/user/" + userId}>
              <MenuItem onClick={popupState.close}>
                <UserWidget />
              </MenuItem>
            </LinkWidget>
            <Divider />
            <LinkWidget
              to={"/settings"} // FIXME
            >
              <MenuItem onClick={popupState.close}>
                <ListItemIcon>
                  <SettingsIcon fontSize="small" />
                </ListItemIcon>
                Settings
              </MenuItem>
            </LinkWidget>
            <MenuItem
              onClick={async () => {
                await logout();
                popupState.close();
              }}
            >
              <ListItemIcon>
                <LogoutIcon fontSize="small" />
              </ListItemIcon>
              Logout
            </MenuItem>
          </Menu>
        </>
      )}
    </PopupState>
  );
};
