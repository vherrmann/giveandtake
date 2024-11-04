import { Avatar, SxProps, Theme } from "@mui/material";
import { LinkWidget } from "./LinkWidget";
import { Api, UserPublic } from "../api";
import { useEffect, useState } from "react";
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
  const api = Api();

  const fetchUserPublic = async () => {
    const response = await api.apiUsersIdGet(userId);
    setUserPublicHere(response.data);
  };

  useEffect(() => {
    if (userPublic === undefined) {
      fetchUserPublic();
    }
  }, []);

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
