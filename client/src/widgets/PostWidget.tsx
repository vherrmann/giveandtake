// import styles
import { ApiPost } from "../api";
import { ViewablePostWidget } from "./ViewablePostWidget";
import { UnviewablePostWidget } from "./UnviewablePostWidget";
import { PostCard } from "./PostCard";
import { Box } from "@mui/material";
import { ComponentProps } from "react";
import { apiPostSwitch } from "../utils";
import { StandardCard } from "./StandardCard";

export const PostWidget = ({
  post,
  postId,
  refetch,
  onDelete,
  cardProps,
}: {
  post: ApiPost;
  postId: string | null;
  refetch: () => void;
  onDelete?: (postId: string) => void;
  cardProps?: ComponentProps<typeof StandardCard>;
}) => {
  return apiPostSwitch({
    post,
    onUnhidden: (data) => (
      <ViewablePostWidget
        vpost={data.post}
        postId={postId}
        onDelete={onDelete}
        usedToUnlock={data.usedToUnlock}
        refetch={refetch}
        cardProps={cardProps}
      />
    ),
    onDeleted: (data) => (
      <PostCard
        userId={data.user}
        title={
          <Box component="span" sx={{ fontStyle: "italic" }}>
            Deleted Post
          </Box>
        }
        createdAt={data.createdAt}
        {...cardProps}
      />
    ),
    onUnlocked: (data) => (
      <ViewablePostWidget
        vpost={data.post}
        postId={postId}
        onDelete={onDelete}
        unlockedWithPost={data.unlockedWithPost}
        refetch={refetch}
        cardProps={cardProps}
      />
    ),
    onLocked: (data) => (
      <UnviewablePostWidget
        post={data}
        postId={postId}
        refetch={refetch}
        cardProps={cardProps}
      />
    ),
  });
};
