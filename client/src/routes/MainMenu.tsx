import HomeIcon from "@mui/icons-material/Home";
import { Link } from "react-router-dom";
import { BottomNavigation, BottomNavigationAction } from "@mui/material";
import GroupsIcon from "@mui/icons-material/Groups";
import AccountCircleIcon from "@mui/icons-material/AccountCircle";
import CreateIcon from "@mui/icons-material/Create";
import NotificationsIcon from "@mui/icons-material/Notifications";
import { useLocation } from "react-router";
import { useAuthedState } from "../ProtectedRoute";

// https://mui.com/material-ui/react-list/
export default function MainMenu() {
  const location = useLocation();
  const { userId } = useAuthedState();

  return (
    <BottomNavigation value={location.pathname}>
      <BottomNavigationAction
        icon={<HomeIcon />}
        label="Home"
        component={Link}
        to="/"
        value="/"
      />
      <BottomNavigationAction
        icon={<GroupsIcon />}
        label="Community"
        component={Link}
        to="/community"
        value="/community"
      />
      <BottomNavigationAction
        icon={<AccountCircleIcon />}
        label="Profile"
        component={Link}
        to={"/user/" + userId}
        value={"/user/" + userId}
      />
      <BottomNavigationAction
        icon={<CreateIcon />}
        label="Create"
        component={Link}
        to="/newpost"
        value="/newpost"
      />
    </BottomNavigation>
  );
}
