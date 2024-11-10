import {
  Avatar,
  CardHeader,
  Divider,
  IconButton,
  List,
  ListItem,
  ListItemAvatar,
  ListItemButton,
  ListItemText,
  Typography,
} from "@mui/material";
import { Link } from "react-router-dom";
import { LinkWidget } from "./LinkWidget";
import { StandardCard } from "./StandardCard";
import { DApi, ErrorWidget, useApiState } from "../utils";
import AddIcon from "@mui/icons-material/Add";

export const GroupsWidget = () => {
  const [errorG, groups] = useApiState(DApi.apiGroupsGet);

  return (
    <StandardCard>
      <CardHeader
        title={
          <Typography gutterBottom variant="h5" align="center">
            Groups
          </Typography>
        }
      />

      <Divider />
      <ErrorWidget errors={[errorG]} />
      <List>
        {groups?.map(({ key: groupId, value: group }) => (
          <ListItem key={groupId}>
            <ListItemAvatar>
              <Avatar sx={{ bgcolor: "#FF6666" }}>
                {
                  "?" // FIXME, use AvatarWidget
                }
              </Avatar>
            </ListItemAvatar>
            <LinkWidget to={"/group/" + groupId}>
              <ListItemText primary={group.name} sx={{ fontWeight: "bold" }} />
            </LinkWidget>
          </ListItem>
        )) ?? "Loading..."}
        <ListItem key="newGroup" disablePadding>
          <ListItemButton component={Link} to={`/newgroup`}>
            <IconButton>
              <AddIcon />
            </IconButton>
            <ListItemText
              primary={"Create new group"}
              sx={{ fontWeight: "bold" }}
            />
          </ListItemButton>
        </ListItem>
      </List>
    </StandardCard>
  );
};
