import { Link } from "react-router-dom";
import { Link as MuiLink, SxProps, Theme } from "@mui/material";
import { ReactNode } from "react";

export const LinkWidget = ({
  to,
  sx,
  children,
}: {
  to: string;
  sx?: SxProps<Theme>;
  children: ReactNode;
}) => {
  return (
    <MuiLink component={Link} to={to} sx={sx} underline="none" color="inherit">
      {children}
    </MuiLink>
  );
};
