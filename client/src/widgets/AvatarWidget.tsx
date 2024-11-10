import { Avatar, SxProps, Theme } from "@mui/material";
import { LinkWidget } from "./LinkWidget";
import { Api, UserPublic } from "../api";
import { useCallback, useEffect, useState } from "react";
import { stringToColor } from "../utils";

export const AvatarWidget = ({
  userId,
  nolink,
  userPublic,
  sx,
}: {
  userId: string;
  nolink?: boolean;
  // if userPublic is undefined, AvatarWidget fetches it
  // if userPublic is null, the parent will(/or can) fetch it
  userPublic?: UserPublic | null;
  sx?: SxProps<Theme>;
}) => {
  const [userPublicHere, setUserPublicHere] = useState<UserPublic | null>(null);

  const fetchUserPublic = useCallback(async () => {
    const response = await Api.apiUsersIdGet({ id: userId });
    setUserPublicHere(response.data);
  }, [userId]);

  useEffect(() => {
    if (userPublic === undefined) {
      fetchUserPublic();
    }
  }, [userPublic, fetchUserPublic]);

  const userPublicBoth = userPublic || userPublicHere;

  const avatar = (
    <Avatar sx={{ ...sx, bgcolor: stringToColor(userId) }}>
      {userPublicBoth ? userPublicBoth.name[0] : "?"}
    </Avatar>
  );

  if (nolink) {
    return avatar;
  } else {
    return <LinkWidget to={"/user/" + userId}>{avatar}</LinkWidget>;
  }
};
