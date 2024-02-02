import Box from "@mui/material/Box";
import List from "@mui/material/List";
import ListItem from "@mui/material/ListItem";
import ListItemButton from "@mui/material/ListItemButton";
import ListItemIcon from "@mui/material/ListItemIcon";
import ListItemText from "@mui/material/ListItemText";
import HomeIcon from "@mui/icons-material/Home";
import { Link } from "react-router-dom";
import { Divider } from "@mui/material";

// https://mui.com/material-ui/react-list/
export default function MainMenu() {
  return (
    <Box
      sx={{
        width: "100%",
        bgcolor: "background.paper",
        height: "100%",
      }}
    >
      <List>
        <nav aria-label="home">
          <ListItem disablePadding>
            <ListItemButton component={Link} to={"/"}>
              <ListItemIcon>
                <HomeIcon />
              </ListItemIcon>
              <ListItemText primary="Home" />
            </ListItemButton>
          </ListItem>
        </nav>
        <Divider />
        <nav aria-label="exercises">
          <ListItem disablePadding>
            <ListItemButton component={Link} to={"/exercise1"}>
              <ListItemText primary="Exercise 1" />
            </ListItemButton>
          </ListItem>
        </nav>
      </List>
    </Box>
  );
}
