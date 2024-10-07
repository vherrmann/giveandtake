import { Card, CardTypeMap } from "@mui/material";
import { DefaultComponentProps } from "@mui/material/OverridableComponent";

export const StandardCard = (props: DefaultComponentProps<CardTypeMap>) => {
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
