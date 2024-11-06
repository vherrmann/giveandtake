import {
  Box,
  Card,
  CardActions,
  CardContent,
  CardHeader,
  CardMedia,
  Dialog,
  DialogContent,
  DialogTitle,
  IconButton,
  List,
  ListItem,
  ListItemButton,
  Menu,
  MenuItem,
  Tooltip,
  Typography,
  useTheme,
  Popover,
} from "@mui/material";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";
import CreateIcon from "@mui/icons-material/Create";
import SyncAltIcon from "@mui/icons-material/SyncAlt";
import CloseIcon from "@mui/icons-material/Close";
import MoreVertIcon from "@mui/icons-material/MoreVert";
import LockIcon from "@mui/icons-material/Lock";
import LockOpen from "@mui/icons-material/LockOpen";
import { ReactNode, useEffect, useRef, useState } from "react";
import Markdown from "react-markdown";
import { Swiper, SwiperSlide } from "swiper/react";
import { Navigation, Pagination, Keyboard } from "swiper/modules";
import "swiper/css";
import "swiper/css/navigation";
import "swiper/css/pagination";

import "./swiper.css";
import ReactPlayer from "react-player";
import LightGallery from "lightgallery/react";
import lgZoom from "lightgallery/plugins/zoom";

// import styles
import "lightgallery/css/lightgallery.css";
import "lightgallery/css/lg-zoom.css";
import { formatDate, handleApiErr } from "../utils";
import PopupState, {
  bindTrigger,
  bindMenu,
  bindPopover,
} from "material-ui-popup-state";
import {
  Api,
  ApiPost,
  LockedHiddenPostData,
  Post,
  UnhiddenPostData,
  User,
  WithUUIDPost,
} from "../api";
import { ShareMenu } from "./ShareMenuWidget";
import { AvatarWidget } from "./AvatarWidget";
import { useAuthedState } from "../ProtectedRoute";
import { Link, useLocation } from "react-router-dom";
import { LinkWidget } from "./LinkWidget";

export interface PostActions {
  deletePost: (postId: string | null) => Promise<void>;
}

const MediaPostWidget = ({ files }: { files: (File | null)[] }) => {
  const theme = useTheme();
  const paginationRef = useRef<HTMLDivElement>(null);

  return (
    <CardMedia
      sx={{
        ["--swiper-theme-color" as any]: theme.palette.primary.main,
      }}
    >
      <Swiper
        spaceBetween={10}
        navigation={true}
        pagination={{
          clickable: true,
          el: paginationRef.current,
          // type: "progressbar",
        }}
        keyboard={{ enabled: true }}
        modules={[Navigation, Pagination, Keyboard]}
        style={{
          aspectRatio: 1, //square
        }}
      >
        {files.map((file) => {
          if (!file) {
            return "Loading...";
          }
          return (
            <SwiperSlide key={file.name}>
              {(() => {
                const fileType = file.type; // Get the MIME type
                if (fileType.startsWith("image/")) {
                  return (
                    <LightGallery plugins={[lgZoom]}>
                      <img src={URL.createObjectURL(file)} />
                    </LightGallery>
                  );
                } else if (fileType.startsWith("video/")) {
                  return (
                    <ReactPlayer
                      url={URL.createObjectURL(file)}
                      controls
                      width="100%"
                      height="100%"
                    />
                  );
                } else {
                  console.log("This is neither an image nor a video."); // FIXME: throw error, remove from list (remove at addition)
                }
              })()}
            </SwiperSlide>
          );
        })}
      </Swiper>
      <Box
        ref={paginationRef}
        style={{
          display: "flex",
          justifyContent: "center",
          alignItems: "center",
          marginTop: "4px",
        }}
      ></Box>
    </CardMedia>
  );
};

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
  postActions,
  unlockedWithPost,
  usedToUnlock,
}: {
  post: Post;
  postId: string | null;
  postActions: PostActions;
  unlockedWithPost?: WithUUIDPost;
  usedToUnlock?: WithUUIDPost[];
}) => {
  const [files, setFiles] = useState<(File | null)[]>(
    post.media.map((_: string) => null),
  );
  const api = Api();
  const { userId } = useAuthedState();

  const [error, setError] = useState<string>("");

  const fetchFiles = async () => {
    try {
      const responses = await Promise.all(
        post.media.map(async (fileId: string) => {
          try {
            const file = api.apiMediaIdGet({ id: fileId });
            return file;
          } catch (err) {
            // FIXME: add error handling
            return null;
          }
        }),
      );
      setFiles(responses.map((response) => response?.data ?? null));
    } catch (err) {
      setError("Failed to fetch posts");
    }
  };

  useEffect(() => {
    fetchFiles();
  }, [post.media]);

  // TODO: add endpoint
  /* const fetchUser = async () => {
   *   try {
   *     const user = await api.apiUsers ({ id: post.user });
   *     setUser(user);
   *   } catch (err) {
   *     setError("Failed to fetch user");
   *   }
   * }; */

  const postUrl = `${window.location.origin}/post/${postId}`;

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
                <MenuItem
                  onClick={async () => {
                    popupState.close();
                    await postActions.deletePost(postId);
                  }}
                  disabled={post.user !== userId}
                >
                  Delete post
                </MenuItem>
              </Menu>
            </>
          )}
        </PopupState>
      }
      content={
        <>
          {files.length != 0 && <MediaPostWidget files={files} />}
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
          <CardActions disableSpacing>
            {
              // FIXME: add like functionality
              /* <IconButton aria-label="add to favorites">
          <FavoriteIcon />
        </IconButton> */
            }
            <ShareMenu url={postUrl} />
            {usedToUnlock && usedToUnlock.length !== 0 && (
              <PopupState variant="popover" popupId="show-trades">
                {(popupState) => (
                  <>
                    <IconButton {...bindTrigger(popupState)}>
                      <SyncAltIcon />
                    </IconButton>
                    <Popover {...bindPopover(popupState)}>
                      <List>
                        {usedToUnlock.map((postW) => {
                          const post = postW.value;
                          return (
                            <ListItem key={postW.key}>
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
  const [loading, setLoading] = useState(true);
  const [tradeablePosts, setTradeablePosts] = useState<WithUUIDPost[]>([]);
  const api = Api();

  const location = useLocation();

  const fetchTradeables = async () => {
    try {
      try {
        const response = await api.apiPostsTradeablesUserGet({
          user: post.user,
        });
        setTradeablePosts(response.data);
      } catch (e) {
        setError(handleApiErr(e));
      }
      setLoading(false);
    } catch (err) {
      setError("Failed to fetch posts");
    }
  };

  useEffect(() => {
    fetchTradeables();
  }, []);

  const tradeExisting = (tradingWithPostId: string) => async () => {
    if (!postId) {
      console.log("PostId null");
      return;
    }
    try {
      await api.apiPostsTradeWithPostForPostPost({
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
            <DialogContent dividers>
              {error && <Typography color="error">Error: {error}</Typography>}
              {loading && <Typography>Loading...</Typography>}
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
                <ListItemButton
                  key="newPost"
                  component={Link}
                  to={`/newpost/?tradeFor=${postId}`}
                  state={{ path: location.pathname }}
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
                {tradeablePosts.map((postW) => {
                  const post = postW.value;
                  // FIXME: make posts clickable to see media
                  return (
                    <ListItem key={postW.key}>
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
            </DialogContent>
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
  postActions,
}: {
  post: ApiPost;
  postId: string | null;
  postActions: PostActions;
}) => {
  switch (post.tag) {
    case "UnhiddenPost":
      return (
        <ViewablePostWidget
          post={post.contents.post}
          postId={postId}
          postActions={postActions}
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
              postActions={postActions}
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
