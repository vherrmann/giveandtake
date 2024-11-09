import React from "react";
import ReactDOM from "react-dom/client";
import "./index.css";
import Frame from "./routes/Frame";
import reportWebVitals from "./reportWebVitals";
import { createBrowserRouter, Outlet, RouterProvider } from "react-router-dom";
import ErrorPage from "./error-page";
import { ThemeProvider } from "@mui/material/styles";
import { theme } from "./theme"; // Path to your custom theme
import { Login } from "./routes/Login";
import NewPostRoute from "./routes/NewPostRoute";
import PostRoute from "./routes/PostRoute";
import { ProtectedRoute } from "./ProtectedRoute";
import { AuthProvider } from "./providers/auth";
import { Signup } from "./routes/Signup";
import { VerifyEmail } from "./routes/VerifyEmail";
import CssBaseline from "@mui/material/CssBaseline/CssBaseline";
import UserRoute from "./routes/UserRoute";
import CommunityRoute from "./routes/CommunityRoute";
import HomeRoute from "./routes/HomeRoute";
import { SettingsRoute } from "./routes/SettingsRoute";
import NewGroupRoute from "./routes/NewGroupRoute";
import GroupRoute from "./routes/GroupRoute";
import { ConfirmProvider } from "material-ui-confirm";

// Rickroll banner
const banner = `
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⣤⣄⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣴⣶⣿⣿⣿⣿⣿⣿⣿⣷⣶⣆⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⡀⠀⠀⠀
⠀⣤⣄⠀⢠⣤⢤⣤⣤⣤⣄⣤⣤⡄⢠⣤⣤⣤⣤⣤⣄⣤⣤⣤⣄⠀⠀⣸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢿⣿⣶⣷⠀⠀⠀
⠀⣿⣿⣧⣈⡿⢹⣿⠏⡝⠟⢿⣿⡅⣼⠏⢸⣿⢉⡙⠇⣿⡟⣽⣿⠀⠀⣿⣿⣿⠟⠉⠀⠀⠀⠉⠁⠀⠀⠀⠙⢿⣿⣿⠀⠀⠀
⠀⣿⢿⡿⣿⡇⢸⣿⢾⡇⠀⢸⣿⣷⡟⠀⣼⣿⢾⠃⢠⣿⡷⣿⣏⠀⠀⣿⣿⡏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⣿⣿⡄⠀⠀
⣰⣿⡀⠻⣶⢇⣾⣿⣈⣴⠇⠘⣶⡞⠀⢀⣷⣇⣈⣼⣻⣿⡁⣿⣿⡀⠀⣿⣿⣇⣤⠶⠶⣶⣆⠀⢠⣴⣤⣤⡀⠀⠿⣻⠃⠀⠀
⠙⠛⠁⠀⠙⠈⠉⠉⠉⠛⠀⠀⠙⠀⠀⠈⠉⠉⠉⠋⠙⠛⠁⠙⠛⠁⠀⢻⣿⠈⠁⠞⠛⠛⠀⠀⠻⡛⠻⠿⠧⠀⢀⠇⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⠟⡻⡄⠀⠀⠀⠀⢀⣀⣀⠁⠀⠀⠀⠀⣰⣵⠀⠀⠀
⠀⢀⣤⣤⣤⠀⢀⣤⣤⡀⢠⣤⡀⠀⣤⣤⣤⣄⠀⢠⣤⡄⠀⣠⡄⠀⠈⢆⠁⢿⠀⠀⠀⠀⠉⠛⠛⠛⠀⠀⠀⠀⡿⡝⠀⠀⠀
⣰⣿⠋⠹⣿⢠⣿⠋⢿⣿⡌⣿⣿⣆⢹⡏⢻⣿⣷⡈⣿⠁⢀⣿⣿⡄⠀⠈⠣⣼⡄⠀⠀⢴⣶⣿⣻⣷⠤⠀⠀⣸⠜⠀⠀⠀⠀
⣿⣿⢠⣴⣶⣿⣿⡀⢸⣿⡇⣿⢿⣿⣿⡇⣿⡿⣿⣿⣿⠀⣼⣉⣿⣇⠀⠀⠀⠘⣿⡆⠀⠀⠳⠭⠽⠵⠀⠀⢠⠋⠀⠀⠀⠀⠀
⠻⣿⣦⣿⡟⢸⣿⣧⣼⡿⣸⣿⡀⢻⣿⢇⣿⣇⠙⣿⡇⣰⣟⠛⣿⣿⡄⠀⠀⣸⡇⠻⣦⣄⣀⣀⣠⡀⠀⠀⣸⠀⠀⠀⠀⠀⠀
⠀⠙⠛⠋⠀⠀⠙⠛⠋⠀⠙⠛⠁⠀⠙⠈⠛⠁⠀⠈⠋⠛⠋⠈⠛⠛⠁⠀⠀⣿⣷⡄⠈⢻⣿⣿⣿⣷⣾⣿⣿⡄⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢿⣿⣿⣦⠼⠿⠿⠿⠿⠿⣿⣿⡷⠀⠀⠀⠀⠀
⠀⠀⣀⣠⣀⡀⣀⣀⣀⣀⣀⡀⣀⣀⣀⣀⣀⣠⠀⠀⣀⣀⣀⣀⣀⡀⢀⣀⣀⡀⣀⣀⣀⣀⣀⡀⠀⣀⣀⣀⢀⣀⣀⣀⣀⣀⡀
⢀⣾⣿⠋⢿⠇⢹⣿⡏⢹⣿⡇⣽⠏⢹⣿⢫⡽⠃⠀⠙⢿⣯⣾⠋⣴⣿⠋⣿⣷⢹⣿⠏⠘⣿⠁⠀⢹⣿⡏⠈⣿⠉⣿⡏⣿⣿
⢸⣿⣇⢶⣶⣶⣸⣿⠃⠈⣿⣷⡟⠀⣼⣿⢻⢧⡀⠀⠀⢸⣿⡇⢰⣿⣿⢀⣿⡿⣾⣿⠀⢸⡿⠀⠀⣼⣿⠀⢰⡿⢰⣿⡿⠟⠃
⠀⠻⠿⠿⠛⠱⠿⠿⠆⠀⠹⠟⠀⠰⠿⠿⠶⠿⠃⠀⠀⠿⠿⠗⠀⠻⠿⠾⠟⠁⠙⠿⠿⠿⠃⠀⠀⠙⠿⠿⠿⠁⠿⠿⠷⠀⠀
`;
console.log(
  `%c${banner}`,
  "color: #007bff; font-family: monospace; font-size: 1.5em",
);
console.log(
  "%cWelcome to the console! If you're here, you probably know what you're doing. If you don't, be careful!",
  "color: #007bff; font-family: monospace; font-size: 1em",
);

// Setup anti XSRF-TOKEN. See also
// https://github.com/haskell-servant/servant/tree/master/servant-auth
/* {
 *   // Get the XSRF token from cookies
 *   const token = (() => {
 *     const match = document.cookie.match(new RegExp("XSRF-TOKEN=([^;]+)"));
 *     return match ? match[1] : null;
 *   })();
 *
 *   // Add a request interceptor
 *   axios.interceptors.request.use(
 *     (config) => {
 *       if (token) {
 *         // Set the XSRF token header
 *         config.headers["X-XSRF-TOKEN"] = token;
 *       }
 *       return config;
 *     },
 *     (error) => {
 *       return Promise.reject(error);
 *     },
 *   );
 * } */
// https://reactrouter.com/en/main/start/tutorial
const router = createBrowserRouter([
  {
    path: "/",
    element: (
      <Frame>
        <Outlet />
      </Frame>
    ),
    errorElement: (
      <Frame>
        <ErrorPage />
      </Frame>
    ),
    children: [
      ProtectedRoute({
        path: "/",
        element: <Outlet />,
        children: [
          { index: true, element: <HomeRoute /> },
          { path: "newpost", element: <NewPostRoute /> },
          { path: "post/:postId", element: <PostRoute /> },
          { path: "user/:userId", element: <UserRoute /> },
          { path: "community", element: <CommunityRoute /> },
          { path: "settings", element: <SettingsRoute /> },
          { path: "newgroup", element: <NewGroupRoute /> },
          { path: "group/:groupId", element: <GroupRoute /> },
        ],
      }),
      { path: "login", element: <Login /> },
      { path: "signup", element: <Signup /> },
      { path: "verifyemail", element: <VerifyEmail /> },
    ],
  },
]);

const root = ReactDOM.createRoot(
  document.getElementById("root") as HTMLElement,
);
root.render(
  <React.StrictMode>
    <ThemeProvider theme={theme}>
      <ConfirmProvider>
        <CssBaseline enableColorScheme />
        <AuthProvider>
          <RouterProvider router={router} />
        </AuthProvider>
      </ConfirmProvider>
    </ThemeProvider>
  </React.StrictMode>,
);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
