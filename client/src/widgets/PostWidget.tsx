// import styles
import { ApiPost } from "../api";
import { ViewablePostWidget } from "./ViewablePostWidget";
import { UnviewablePostWidget } from "./UnviewablePostWidget";
import { PostCard } from "./PostCard";
import { Box } from "@mui/material";
import { ComponentProps } from "react";
import { PostLikeCard } from "./PostLikeCard";
import { apiPostSwitch } from "../utils";

export const PostWidget = ({
  post,
  postId,
  refetch,
  onDelete,
  postLikeCardProps,
}: {
  post: ApiPost;
  postId: string | null;
  refetch: () => void;
  onDelete?: (postId: string) => void;
  postLikeCardProps?: ComponentProps<typeof PostLikeCard>;
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
        postLikeCardProps={postLikeCardProps}
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
        {...postLikeCardProps}
      />
    ),
    onUnlocked: (data) => (
      <ViewablePostWidget
        vpost={data.post}
        postId={postId}
        onDelete={onDelete}
        unlockedWithPost={data.unlockedWithPost}
        refetch={refetch}
        postLikeCardProps={postLikeCardProps}
      />
    ),
    onLocked: (data) => (
      <UnviewablePostWidget
        post={data}
        postId={postId}
        refetch={refetch}
        postLikeCardProps={postLikeCardProps}
      />
    ),
  });
};
