import { Card } from "@mui/material";
import { ComponentProps } from "react";

export const StandardCard = (props: ComponentProps<typeof Card>) => {
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
      {...props}
    ></Card>
  );
};
