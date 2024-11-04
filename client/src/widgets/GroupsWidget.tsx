import { useEffect, useState } from "react";
import { Api, WithUUIDUserPublic } from "../api";
import {
  Avatar,
  Card,
  CardContent,
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
import { Link, useLocation } from "react-router-dom";
import { AvatarWidget } from "./AvatarWidget";
import PersonRemoveIcon from "@mui/icons-material/PersonRemove";
import { LinkWidget } from "./LinkWidget";
import { StandardCard } from "./StandardCard";
import { ListUserItem } from "./ListUserItem";
import { handleApiErr } from "../utils";
import AddIcon from "@mui/icons-material/Add";

export const GroupsWidget = ({ userId }: { userId: string }) => {
  const [groups, setGroups] = useState<WithUUIDUserPublic[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");
  const location = useLocation();

  const api = Api();
  const fetchGroups = async () => {
    try {
      setLoading(true);
      const response = await api.apiGroupsGet();
      const groups = response.data;
      setGroups(groups);
    } catch (e) {
      setError(handleApiErr(e));
    }
    setLoading(false);
  };

  useEffect(() => {
    fetchGroups();
  }, []);

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
      <List>
        {groups.map(({ uuid: groupId, value: group }) => (
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
        ))}
        <ListItem>
          <ListItemButton key="newGroup" component={Link} to={`/newgroup`}>
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
