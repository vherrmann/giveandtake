import { Link } from "react-router-dom";
import { Link as MuiLink, SxProps, Theme } from "@mui/material";
import { ReactNode } from "react";

export const LinkWidget = ({
  to,
  sx,
  underline,
  children,
  color,
}: {
  to: string;
  sx?: SxProps<Theme>;
  children: ReactNode;
  underline?: "none" | "hover" | "always";
  color?: string;
}) => {
  return (
    <MuiLink
      component={Link}
      to={to}
      sx={sx}
      underline={underline || "none"}
      color={color || "inherit"}
    >
      {children}
    </MuiLink>
  );
};
