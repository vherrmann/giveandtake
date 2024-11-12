import { Card } from "@mui/material";
import { ReactNode } from "react";

export const PostLikeCard = ({ children }: { children: ReactNode }) => {
  return (
    <Card
      sx={{
        width: {
          xs: "100vw",
          sm: "75vw",
          md: "50vw",
          lg: "33vw",
          xl: "25vw",
        },
        height: "auto",
      }}
    >
      {children}
    </Card>
  );
};
