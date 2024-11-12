import { CardHeader, Typography } from "@mui/material";
import { ReactNode } from "react";

// import styles
import { formatDate } from "../utils";
import { UserAvatarWidget } from "./UserAvatarWidget";
import { PostLikeCard } from "./PostLikeCard";

export const PostCard = ({
  userId,
  title,
  createdAt,
  headerActions,
  content,
}: {
  userId: string;
  title: ReactNode;
  createdAt: string;
  headerActions?: ReactNode;
  content?: ReactNode;
}) => {
  return (
    <PostLikeCard>
      <CardHeader
        avatar={<UserAvatarWidget userId={userId} />}
        action={headerActions}
        title={<Typography sx={{ fontWeight: "bold" }}>{title}</Typography>}
        subheader={formatDate(createdAt)}
      />
      {content}
    </PostLikeCard>
  );
};
