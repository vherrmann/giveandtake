import { Card } from "@mui/material";
import { ComponentProps, ReactNode } from "react";

export const StandardCard = ({
  children,
  ...props
}: { children?: ReactNode } & ComponentProps<typeof Card>) => {
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
        ...props.sx,
      }}
      {...props}
    >
      {children}
    </Card>
  );
};
