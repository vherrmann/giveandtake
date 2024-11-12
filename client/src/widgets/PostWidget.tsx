// import styles
import { ApiPost } from "../api";
import { ViewablePostWidget } from "./ViewablePostWidget";
import { UnviewablePostWidget } from "./UnviewablePostWidget";
import { PostCard } from "./PostCard";
import { Box } from "@mui/material";

export const PostWidget = ({
  post,
  postId,
  refetch,
  onDelete,
}: {
  post: ApiPost;
  postId: string | null;
  refetch: () => void;
  onDelete?: (postId: string) => void;
}) => {
  switch (post.tag) {
    case "UnhiddenPost":
      return (
        <ViewablePostWidget
          vpost={post.contents.post}
          postId={postId}
          onDelete={onDelete}
          usedToUnlock={post.contents.usedToUnlock}
          refetch={refetch}
        />
      );
    case "DeletedPost":
      return (
        <PostCard
          userId={post.contents.user}
          title={
            <Box component="span" sx={{ fontStyle: "italic" }}>
              Deleted Post
            </Box>
          }
          createdAt={post.contents.createdAt}
        />
      );
    case "HiddenPost":
      const hpData = post.contents;
      switch (hpData.tag) {
        case "UnlockedHiddenPost":
          return (
            <ViewablePostWidget
              vpost={hpData.contents.post}
              postId={postId}
              onDelete={onDelete}
              unlockedWithPost={hpData.contents.unlockedWithPost}
              refetch={refetch}
            />
          );
        case "LockedHiddenPost":
          return (
            <UnviewablePostWidget
              post={hpData.contents}
              postId={postId}
              refetch={refetch}
            />
          );
      }
  }
};
