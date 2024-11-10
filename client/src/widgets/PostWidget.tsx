import {
  Box,
  Card,
  CardActions,
  CardContent,
  CardHeader,
  Dialog,
  DialogTitle,
  IconButton,
  List,
  ListItem,
  ListItemButton,
  Menu,
  MenuItem,
  Tooltip,
  Typography,
  Popover,
  Divider,
} from "@mui/material";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";
import CreateIcon from "@mui/icons-material/Create";
import SyncAltIcon from "@mui/icons-material/SyncAlt";
import CloseIcon from "@mui/icons-material/Close";
import MoreVertIcon from "@mui/icons-material/MoreVert";
import LockIcon from "@mui/icons-material/Lock";
import LockOpen from "@mui/icons-material/LockOpen";
import { ReactNode, useState } from "react";
import Markdown from "react-markdown";

// import styles
import {
  DApi,
  ErrorWidget,
  formatDate,
  handleApiErr,
  useApi,
  useApiState,
} from "../utils";
import PopupState, {
  bindTrigger,
  bindMenu,
  bindPopover,
} from "material-ui-popup-state";
import { Api, ApiPost, LockedHiddenPostData, Post, WithUUIDPost } from "../api";
import { ShareMenu } from "./ShareMenuWidget";
import { AvatarWidget } from "./AvatarWidget";
import { useAuthedState } from "../ProtectedRoute";
import { Link, useLocation } from "react-router-dom";
import { LinkWidget } from "./LinkWidget";
import { MediaPostWidget } from "./MediaPostWidget";
import { useConfirm } from "material-ui-confirm";

const PostLikeCard = ({ children }: { children: ReactNode }) => {
  return (
    <Card
      sx={{
        width: {
          xs: "100vw",
          sm: "75vw",
          md: "50vw",
          lg: "33vw",
          xl: "25vw",
        },
        height: "auto",
      }}
    >
      {children}
    </Card>
  );
};

const PostCard = ({
  user,
  title,
  createdAt,
  headerActions,
  content,
}: {
  user: string;
  title: string;
  createdAt: string;
  headerActions: ReactNode;
  content: ReactNode;
}) => {
  return (
    <PostLikeCard>
      <CardHeader
        avatar={<AvatarWidget userId={user} />}
        action={headerActions}
        title={<Typography sx={{ fontWeight: "bold" }}>{title}</Typography>}
        subheader={formatDate(createdAt)}
      />
      {content}
    </PostLikeCard>
  );
};

const ViewablePostWidget = ({
  post,
  postId,
  onDelete,
  unlockedWithPost,
  usedToUnlock,
}: {
  post: Post;
  postId: string | null;
  onDelete?: (postId: string) => void;
  unlockedWithPost?: WithUUIDPost;
  usedToUnlock?: WithUUIDPost[];
}) => {
  const { userId } = useAuthedState();
  const confirm = useConfirm();

  const postUrl = `${window.location.origin}/post/${postId}`;

  const [errorPD, deletePost] = useApi(DApi.apiPostsIdDelete);

  return (
    <PostCard
      user={post.user}
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
            {
              // FIXME: add like functionality
              /* <IconButton aria-label="add to favorites">
          <FavoriteIcon />
        </IconButton> */
            }
            <ShareMenu url={postUrl} title={post.title} />
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
                                user={post.user}
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
    />
  );
};

const UnviewablePostWidget = ({
  post,
  postId,
}: {
  post: LockedHiddenPostData;
  postId: string | null;
}) => {
  const [unlockDOpen, setUnlockDOpen] = useState(false);
  const handleClose = (_: string) => {
    setUnlockDOpen(false);
  };
  const [error, setError] = useState<string>("");
  const location = useLocation();

  const [errorTP, tradeablePosts] = useApiState(
    DApi.apiPostsTradeablesUserGet,
    {
      user: post.user,
    },
  );

  const tradeExisting = (tradingWithPostId: string) => async () => {
    if (!postId) {
      console.log("PostId null");
      return;
    }
    try {
      await Api.apiPostsTradeWithPostForPostPost({
        withPost: tradingWithPostId,
        forPost: postId,
      });
      setUnlockDOpen(false);
      // FIXME: reload only current post
      window.location.reload();
    } catch (e) {
      setError(handleApiErr(e));
    }
  };

  return (
    <PostCard
      user={post.user}
      title={post.title}
      createdAt={post.createdAt}
      headerActions={
        <>
          <IconButton onClick={() => setUnlockDOpen(true)}>
            <Tooltip title="Unlock post">
              <LockIcon />
            </Tooltip>
          </IconButton>
          <Dialog open={unlockDOpen} onClose={handleClose}>
            <DialogTitle sx={{ m: 0, p: 1 }}>
              <IconButton>
                <LockOpen />
              </IconButton>
              Unlock by trading with
            </DialogTitle>
            <IconButton
              aria-label="close"
              onClick={() => setUnlockDOpen(false)}
              sx={(theme) => ({
                position: "absolute",
                right: 8,
                top: 8,
                color: theme.palette.grey[500],
              })}
            >
              <CloseIcon />
            </IconButton>
            <Divider />
            <ErrorWidget errors={[error, errorTP]} />
            {!tradeablePosts && <Typography>Loading...</Typography>}
            <List
              sx={{
                width: "100%",
                maxWidth: 360,
                position: "relative",
                overflow: "auto",
                maxHeight: 300,
                scrollbarWidth: "thin", // Firefox
                "&::-webkit-scrollbar": {
                  width: "8px",
                },
                "&::-webkit-scrollbar-thumb": {
                  backgroundColor: "#888",
                  borderRadius: "4px",
                },
              }}
            >
              <ListItem key="newPost" disableGutters disablePadding>
                <ListItemButton
                  component={Link}
                  to={`/newpost/?tradeFor=${postId}`}
                  state={{ path: location.pathname }}
                  disableGutters
                >
                  <PostLikeCard>
                    <CardHeader
                      avatar={<CreateIcon />}
                      action={
                        // Add tooltip
                        <IconButton>
                          <SyncAltIcon />
                        </IconButton>
                      }
                      title={
                        <Typography sx={{ fontWeight: "bold" }}>
                          New post
                        </Typography>
                      }
                    />
                  </PostLikeCard>
                </ListItemButton>
              </ListItem>
              {tradeablePosts?.map((postW) => {
                const post = postW.value;
                // FIXME: make posts clickable to see media
                return (
                  <ListItem key={postW.key} disableGutters disablePadding>
                    <PostCard
                      user={post.user}
                      title={post.title}
                      createdAt={post.createdAt}
                      headerActions={
                        <>
                          <IconButton
                            component={Link}
                            to={"/post/" + postW.key}
                            target="_blank"
                            rel="noopener noreferrer"
                          >
                            <OpenInNewIcon />
                          </IconButton>
                          <IconButton onClick={tradeExisting(postW.key)}>
                            <SyncAltIcon />
                          </IconButton>
                        </>
                      }
                      content={<> </>}
                    />
                  </ListItem>
                );
              })}
            </List>
          </Dialog>
        </>
      }
      content={<></>}
    />
  );
};

export const PostWidget = ({
  post,
  postId,
  onDelete,
}: {
  post: ApiPost;
  postId: string | null;
  onDelete?: (postId: string) => void;
}) => {
  switch (post.tag) {
    case "UnhiddenPost":
      return (
        <ViewablePostWidget
          post={post.contents.post}
          postId={postId}
          onDelete={onDelete}
          usedToUnlock={post.contents.usedToUnlock}
        />
      );
    case "HiddenPost":
      const hpData = post.contents;
      switch (hpData.tag) {
        case "UnlockedHiddenPost":
          return (
            <ViewablePostWidget
              post={hpData.contents.post}
              postId={postId}
              onDelete={onDelete}
              unlockedWithPost={hpData.contents.unlockedWithPost}
            />
          );
        case "LockedHiddenPost":
          return (
            <UnviewablePostWidget post={hpData.contents} postId={postId} />
          );
      }
  }
};
