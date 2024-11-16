import { CardHeader, Typography } from "@mui/material";
import { ComponentProps, ReactNode } from "react";

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
  postLikeCardProps,
}: {
  userId: string;
  title: ReactNode;
  createdAt: string;
  headerActions?: ReactNode;
  content?: ReactNode;
  postLikeCardProps?: ComponentProps<typeof PostLikeCard>;
}) => {
  return (
    <PostLikeCard {...postLikeCardProps}>
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
