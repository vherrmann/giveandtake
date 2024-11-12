import {
  CardHeader,
  Dialog,
  DialogTitle,
  IconButton,
  List,
  ListItem,
  ListItemButton,
  Tooltip,
  Typography,
  Divider,
} from "@mui/material";
import OpenInNewIcon from "@mui/icons-material/OpenInNew";
import CreateIcon from "@mui/icons-material/Create";
import SyncAltIcon from "@mui/icons-material/SyncAlt";
import CloseIcon from "@mui/icons-material/Close";
import LockIcon from "@mui/icons-material/Lock";
import LockOpen from "@mui/icons-material/LockOpen";
import { useState } from "react";

// import styles
import { DApi, ErrorWidget, handleApiErr, useApiState } from "../utils";
import { Api, LockedHiddenPostData } from "../api";
import { Link, useLocation } from "react-router-dom";
import { PostCard } from "./PostCard";
import { PostLikeCard } from "./PostLikeCard";

export const UnviewablePostWidget = ({
  post,
  postId,
  refetch,
}: {
  post: LockedHiddenPostData;
  postId: string | null;
  refetch: () => void;
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
      refetch();
    } catch (e) {
      setError(handleApiErr(e));
    }
  };

  return (
    <PostCard
      userId={post.user}
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
                      userId={post.user}
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
