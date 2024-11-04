import {
  Box,
  Button,
  IconButton,
  Popover,
  Tooltip,
  useTheme,
} from "@mui/material";
import AppBar from "@mui/material/AppBar";
import Toolbar from "@mui/material/Toolbar";
import Typography from "@mui/material/Typography";
import { useAuthState } from "../providers/auth";
import { useLocation, useNavigate } from "react-router";
import { redirect } from "../utils";
import { LinkWidget } from "./LinkWidget";

import HelpIcon from "@mui/icons-material/Help";
import QrCodeIcon from "@mui/icons-material/QrCode";
import { FeedWidget } from "./FeedIconWidget";
import { NotificationPopup } from "./NotificationPopup";
import { AvatarMenu } from "./AvatarMenu";
import logo from "../assets/logo.png";
import PopupState, { bindPopover, bindTrigger } from "material-ui-popup-state";
import { QRCodeSVG } from "qrcode.react";

// see https://mui.com/material-ui/react-app-bar/
// see https://mui.com/material-ui/react-app-bar/#fixed-placement about placement

export const AppBarWidget = () => {
  const { isAuthenticated, user, userId } = useAuthState();
  const location = useLocation();
  const navigate = useNavigate();
  const theme = useTheme();

  const navToLogin = () => {
    navigate("/login", {
      state: { from: location.pathname },
      replace: true,
    });
  };

  const loginButton = (
    <Button
      color="inherit"
      onClick={() => {
        navToLogin();
      }}
    >
      Login
    </Button>
  );
  const loggedinBarActions = () => {
    if (!isAuthenticated) {
      throw new Error("User is not authenticated");
    }
    return (
      <>
        <FeedWidget />
        <NotificationPopup />
        <AvatarMenu userId={userId} user={user} />
      </>
    );
  };

  return (
    <AppBar color="primary" style={{ zIndex: 1000 }}>
      <Toolbar>
        <LinkWidget to={"/"}>
          <img src={logo} alt="Logo" style={{ height: 40, marginRight: 16 }} />
        </LinkWidget>
        {
          undefined
          // Takes too much space
          /* <LinkWidget to={"/"}>
          <Typography variant="h6">Give'n'take</Typography>
        </LinkWidget> */
        }
        <Box sx={{ flexGrow: 1 }} />

        <Tooltip
          title="Open documentation" // FIXME: leads to wrong page
        >
          <IconButton onClick={() => redirect("/docs")} aria-label="help">
            <HelpIcon />
          </IconButton>
        </Tooltip>
        <PopupState variant="popover" popupId="qrcode-popover">
          {(popupState) => (
            <>
              <IconButton {...bindTrigger(popupState)} aria-label="qrcode">
                <QrCodeIcon />
              </IconButton>
              <Popover {...bindPopover(popupState)}>
                <QRCodeSVG
                  value={window.location.href}
                  fgColor={theme.palette.primary.main}
                />
              </Popover>
            </>
          )}
        </PopupState>
        {isAuthenticated ? loggedinBarActions() : loginButton}
      </Toolbar>
    </AppBar>
  );
};
