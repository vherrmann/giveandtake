import {
  Box,
  CardActions,
  CardContent,
  IconButton,
  List,
  ListItem,
  Menu,
  MenuItem,
  Typography,
  Popover,
  Divider,
  Tooltip,
} from "@mui/material";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";
import SyncAltIcon from "@mui/icons-material/SyncAlt";
import MoreVertIcon from "@mui/icons-material/MoreVert";
import Markdown from "react-markdown";

// import styles
import { DApi, ErrorWidget, useApi } from "../utils";
import PopupState, {
  bindTrigger,
  bindMenu,
  bindPopover,
} from "material-ui-popup-state";
import { ViewablePostData, WithUUIDPost } from "../api";
import { ShareMenu } from "./ShareMenuWidget";
import { useAuthedState } from "../ProtectedRoute";
import { Link } from "react-router-dom";
import { LinkWidget } from "./LinkWidget";
import { MediaPostWidget } from "./MediaPostWidget";
import { useConfirm } from "material-ui-confirm";
import { PostCard } from "./PostCard";
import Blobhaj from "../assets/Blobhaj.svg";
import BlobhajHeart from "../assets/BlobhajHeart.svg";
import Blobhaj_Party from "../assets/Blobhaj_Party.gif";
import { ComponentProps, CSSProperties, useEffect, useState } from "react";
import { PostLikeCard } from "./PostLikeCard";

// Custom styled Button with the GIF as an icon

const LikeButton = ({
  postId,
  liked,
  refetch,
}: {
  postId: string | null;
  liked: boolean;
  refetch: () => void;
}) => {
  const [showParty, setShowParty] = useState(false);

  useEffect(() => {
    setShowParty(liked);
    if (liked) {
      const id = setTimeout(() => {
        setShowParty(false);
      }, 1000);
      return () => clearTimeout(id);
    }
  }, [liked]);

  const imgToEl = (imgSrc: any, style: CSSProperties | undefined) => (
    <img
      src={imgSrc}
      alt="icon"
      style={{
        width: "24px",
        height: "24px",
        objectFit: "cover",
        ...style,
      }}
    />
  );

  const [errorLP, likePost] = useApi(DApi.apiPostsLikeIdPut, {
    onSuccess: refetch,
  });
  const [errorULP, unlikePost] = useApi(DApi.apiPostsLikeIdDelete, {
    onSuccess: refetch,
  });

  const switchLike = async () => {
    if (postId) {
      if (liked) {
        unlikePost({ id: postId });
      } else {
        likePost({ id: postId });
      }
    }
  };

  return (
    <>
      <IconButton aria-label="like" onClick={switchLike}>
        <Tooltip title={liked ? "Unlike" : "Like"}>
          {imgToEl(
            showParty ? Blobhaj_Party : liked ? BlobhajHeart : Blobhaj,
            liked ? undefined : { filter: "grayscale(100%)" },
          )}
        </Tooltip>
        <ErrorWidget errors={[errorLP, errorULP]} />
      </IconButton>
    </>
  );
};

export const ViewablePostWidget = ({
  vpost,
  postId,
  refetch,
  onDelete,
  unlockedWithPost,
  usedToUnlock,
  postLikeCardProps,
}: {
  vpost: ViewablePostData;
  postId: string | null;
  refetch: () => void;
  onDelete?: (postId: string) => void;
  unlockedWithPost?: WithUUIDPost;
  usedToUnlock?: WithUUIDPost[];
  postLikeCardProps?: ComponentProps<typeof PostLikeCard>;
}) => {
  const { userId } = useAuthedState();
  const confirm = useConfirm();

  const postUrl = `${window.location.origin}/post/${postId}`;

  const [errorPD, deletePost] = useApi(DApi.apiPostsIdDelete, {
    onSuccess: refetch,
  });

  const post = vpost.post;

  return (
    <PostCard
      userId={post.user}
      title={post.title}
      createdAt={post.createdAt}
      headerActions={
        <PopupState variant="popover" popupId="post-action-menu">
          {(popupState) => (
            <>
              <IconButton {...bindTrigger(popupState)}>
                <MoreVertIcon />
              </IconButton>
              <Menu {...bindMenu(popupState)}>
                {post.user === userId && (
                  <MenuItem
                    onClick={async () => {
                      popupState.close();

                      await confirm({
                        title: "Delete post",
                        description:
                          "Are you sure you want to delete this post?",
                        confirmationText: "Delete",
                        cancellationText: "Cancel",
                      });
                      if (postId) {
                        deletePost(
                          { id: postId },
                          { onSuccess: () => onDelete && onDelete(postId) },
                        );
                      }
                    }}
                  >
                    Delete post
                  </MenuItem>
                )}
              </Menu>
            </>
          )}
        </PopupState>
      }
      content={
        <>
          <Divider />
          <MediaPostWidget fileIds={post.media} />
          <CardContent sx={{ pl: 2, pr: 2, pt: 0, pb: 0 }}>
            {unlockedWithPost && (
              <Typography variant="body2" sx={{ color: "text.secondary" }}>
                Traded with post:
                <LinkWidget
                  to={"/post/" + unlockedWithPost.key}
                  underline="always"
                  color="primary"
                >
                  {unlockedWithPost.value.title}
                </LinkWidget>
              </Typography>
            )}
            <Box
              sx={{
                color: "text.secondary",
                wordWrap: "break-word",
                overflowWrap: "break-word",
                whiteSpace: "normal",
              }}
            >
              <Markdown>{post.body}</Markdown>
            </Box>
          </CardContent>
          <ErrorWidget errors={[errorPD]} />
          <CardActions disableSpacing>
            <ShareMenu url={postUrl} title={post.title} />
            {userId !== post.user && (
              <LikeButton
                postId={postId}
                liked={vpost.liked}
                refetch={refetch}
              />
            )}
            {usedToUnlock && usedToUnlock.length !== 0 && (
              <PopupState variant="popover" popupId="show-trades">
                {(popupState) => (
                  <>
                    <IconButton {...bindTrigger(popupState)}>
                      <SyncAltIcon />
                    </IconButton>
                    <Popover {...bindPopover(popupState)}>
                      <List disablePadding>
                        {usedToUnlock.map((postW) => {
                          const post = postW.value;
                          return (
                            <ListItem
                              key={postW.key}
                              disablePadding
                              disableGutters
                            >
                              <PostCard
                                userId={post.user}
                                title={post.title}
                                createdAt={post.createdAt}
                                headerActions={
                                  <IconButton
                                    component={Link}
                                    to={"/post/" + postW.key}
                                    target="_blank"
                                    rel="noopener noreferrer"
                                  >
                                    <OpenInNewIcon />
                                  </IconButton>
                                }
                                content={<> </>}
                              />
                            </ListItem>
                          );
                        })}
                      </List>
                    </Popover>
                  </>
                )}
              </PopupState>
            )}
          </CardActions>
        </>
      }
      postLikeCardProps={postLikeCardProps}
    />
  );
};
