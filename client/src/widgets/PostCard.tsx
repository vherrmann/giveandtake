import { CardHeader, Typography } from "@mui/material";
import { ComponentProps, ReactNode } from "react";

// import styles
import { formatDate } from "../utils";
import { UserAvatarWidget } from "./UserAvatarWidget";
import { StandardCard } from "./StandardCard";

export const PostCard = ({
  userId,
  title,
  createdAt,
  headerActions,
  content,
  cardProps,
}: {
  userId: string;
  title: ReactNode;
  createdAt: string;
  headerActions?: ReactNode;
  content?: ReactNode;
  cardProps?: ComponentProps<typeof StandardCard>;
}) => {
  return (
    <StandardCard {...cardProps}>
      <CardHeader
        avatar={<UserAvatarWidget userId={userId} />}
        action={headerActions}
        title={<Typography sx={{ fontWeight: "bold" }}>{title}</Typography>}
        subheader={formatDate(createdAt)}
      />
      {content}
    </StandardCard>
  );
};
