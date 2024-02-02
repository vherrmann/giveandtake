import "./Root.css";
import { Outlet } from "react-router-dom";
import AppBar from "@mui/material/AppBar";
import Box from "@mui/material/Box";
import Toolbar from "@mui/material/Toolbar";
import Typography from "@mui/material/Typography";
import Button from "@mui/material/Button";
import IconButton from "@mui/material/IconButton";
import MenuIcon from "@mui/icons-material/Menu";
import MainMenu from "./MainMenu";
import { CssBaseline, Grid } from "@mui/material";

// see https://mui.com/material-ui/react-app-bar/
function ButtonAppBar() {
  return (
    <Box sx={{ flexGrow: 1 }}>
      <AppBar position="static" color="primary">
        <Toolbar>
          <Typography variant="h6" component="div" sx={{ flexGrow: 1 }}>
            Gehörbilder
          </Typography>
        </Toolbar>
      </AppBar>
    </Box>
  );
}

function Root() {
  return (
    <Box
      style={{
        backgroundColor: "#ddd",
        height: "100vh",
      }}
    >
      <CssBaseline />
      <ButtonAppBar />
      <Grid container spacing={0} style={{ height: "93%" }}>
        <Grid item xs={2}>
          <MainMenu />
        </Grid>
        <Grid item xs>
          <Outlet />
        </Grid>
      </Grid>
    </Box>
  );
}

export default Root;
