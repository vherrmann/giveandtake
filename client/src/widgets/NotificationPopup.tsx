import {
  Badge,
  IconButton,
  List,
  ListItem,
  ListItemText,
  Popover,
  Tooltip,
} from "@mui/material";
import Typography from "@mui/material/Typography";
import PopupState, { bindPopover, bindTrigger } from "material-ui-popup-state";

import NotificationsIcon from "@mui/icons-material/Notifications";
import { NotifContent } from "../api";
import Markdown from "react-markdown";
import { popupBarStyle } from "../style";
import { DApi, ErrorWidget, useApi, useApiState } from "../utils";

export const NotificationPopup = () => {
  const [errorN, notifications, { refetch }] = useApiState(DApi.apiNotifGet);

  const notifContentToMsg = (content: NotifContent) => {
    return `
Welcome! Your individual feed can be accessed as an rss feed under this [url](${content.url}).
    `;
  };

  const [errorNRP, readNotification] = useApi(DApi.apiNotifReadPost, {
    onSuccess: refetch,
  });
  const markAllasRead = async () => {
    if (!notifications) return;
    readNotification({
      requestBody: notifications.map((n) => n.key),
    });
  };

  const unreadC = notifications?.filter((n) => !n.value.read).length || 0;

  // FIXME: implement notification scrolling widget
  return (
    <PopupState variant="popover" popupId="avatar-menu-popup">
      {(popupState) => (
        <>
          <Tooltip title="Show notifications">
            <IconButton
              aria-label="share"
              {...bindTrigger(popupState)}
              onClick={(e: React.MouseEvent) => {
                bindTrigger(popupState).onClick(e);
                markAllasRead();
              }}
            >
              <Badge
                badgeContent={unreadC === 0 ? null : unreadC}
                color="secondary"
              >
                <NotificationsIcon color="action" />
              </Badge>
            </IconButton>
          </Tooltip>
          <Popover {...bindPopover(popupState)} {...popupBarStyle}>
            <List>
              <ErrorWidget errors={[errorN, errorNRP]} />
              {notifications === null ? (
                <ListItem>
                  <ListItemText>Loading...</ListItemText>
                </ListItem>
              ) : notifications.length === 0 ? (
                <ListItem>
                  <ListItemText>No notifications</ListItemText>
                </ListItem>
              ) : (
                notifications.map((notif) => (
                  <ListItem key={notif.key}>
                    <ListItemText
                      disableTypography
                      primary={
                        <Typography
                          sx={{
                            fontWeight: notif.value.read ? "normal" : "bold",
                          }}
                        >
                          {notif.value.title}
                        </Typography>
                      }
                      secondary={
                        <Markdown>
                          {notifContentToMsg(notif.value.content)}
                        </Markdown>
                      }
                    />
                  </ListItem>
                ))
              )}
            </List>
          </Popover>
        </>
      )}
    </PopupState>
  );
};
