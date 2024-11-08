import MainMenu from "./MainMenu";
import { Box, Container } from "@mui/material";
import { useAuthState } from "../providers/auth";
import { ReactNode } from "react";
import { ScrollProvider } from "../providers/scroll";
import { AppBarWidget } from "../widgets/AppBarWidget";

function Frame({ children }: { children: ReactNode }) {
  const { isAuthenticated } = useAuthState();
  return (
    <Box sx={{ height: "100dvh", display: "flex", flexDirection: "column" }}>
      <AppBarWidget />
      <ScrollProvider>
        <Container
          maxWidth="lg"
          component="main"
          sx={{
            display: "flex",
            flexDirection: "column",
            justifyContent: "center", // maybe remove this
            alignItems: "center", // maybe remove this
            my: 2,
          }}
        >
          {children}
        </Container>
      </ScrollProvider>
      {isAuthenticated ? <MainMenu /> : <></>}
    </Box>
  );
}

export default Frame;
