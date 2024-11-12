import { Avatar } from "@mui/material";
import { LinkWidget } from "./LinkWidget";
import { stringToColor } from "../utils";
import { ComponentProps } from "react";

export type AvatarBaseParams = {
  id: string;
  disableLink?: boolean;
  toBase: string;
} & ComponentProps<typeof Avatar>;

export const AvatarBase = ({
  id,
  disableLink,
  toBase,
  ...props
}: AvatarBaseParams) => {
  const avatarNode = (
    <Avatar {...props} sx={{ bgcolor: stringToColor(id), ...props.sx }} />
  );

  if (disableLink) {
    return avatarNode;
  } else {
    return <LinkWidget to={toBase + id}>{avatarNode}</LinkWidget>;
  }
};
