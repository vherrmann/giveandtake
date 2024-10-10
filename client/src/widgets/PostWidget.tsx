import {
  Avatar,
  Box,
  Card,
  CardActions,
  CardContent,
  CardHeader,
  CardMedia,
  IconButton,
  Menu,
  MenuItem,
  Typography,
  useTheme,
} from "@mui/material";
import ShareIcon from "@mui/icons-material/Share";
import FavoriteIcon from "@mui/icons-material/Favorite";
import MoreVertIcon from "@mui/icons-material/MoreVert";
import { useEffect, useRef, useState } from "react";
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
import { formatDate } from "../utils";
import PopupState, { bindTrigger, bindMenu } from "material-ui-popup-state";
import { Api, Post, User } from "../api";
import { ShareMenu } from "./ShareMenuWidget";
import { AvatarWidget } from "./AvatarWidget";

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
        {files.map((file) =>
          file ? (
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
          ) : (
            "Loading..."
          ),
        )}
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

export const PostWidget = ({
  post,
  postId,
  postActions,
}: {
  post: Post;
  postId: string | null;
  postActions: PostActions;
}) => {
  const [files, setFiles] = useState<(File | null)[]>(
    post.media.map((_: string) => null),
  );
  const api = new Api();

  const [error, setError] = useState<string>("");
  const [user, setUser] = useState<User | null>(null);

  const fetchFiles = async () => {
    try {
      const files = await Promise.all(
        post.media.map(async (fileId: string) => {
          try {
            const file = api.apiMediaIdGet({ id: fileId });
            return file;
          } catch (err) {
            return null;
          }
        }),
      );
      setFiles(files);
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
    <Card
      sx={{
        // FIXME: extract those size settings to a general component
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
      <CardHeader
        avatar={<AvatarWidget userId={post.user} />}
        action={
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
                  >
                    Delete post
                  </MenuItem>
                </Menu>
              </>
            )}
          </PopupState>
        }
        title={
          <Typography sx={{ fontWeight: "bold" }}>{post.title}</Typography>
        }
        subheader={formatDate(post.createdAt)}
      />
      {files.length != 0 && <MediaPostWidget files={files} />}
      <CardContent sx={{ pl: 2, pr: 2, pt: 0, pb: 0 }}>
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
      </CardActions>
    </Card>
  );
};
