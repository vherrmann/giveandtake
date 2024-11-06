import styled from "@emotion/styled";
import { Box, Button, Grid, Stack, TextField, Typography } from "@mui/material";
import CloudUploadIcon from "@mui/icons-material/CloudUpload";
import { ChangeEvent, useEffect, useRef, useState } from "react";

// import styles
import { getCurrDate, handleApiErr, useLocalStorage } from "../utils";
import { PostWidget } from "../widgets/PostWidget";
import { Api, ApiPost, JobStatus, Post, UnhiddenPostTagEnum } from "../api";
import { useAuthedState } from "../ProtectedRoute";
import { useLocation, useNavigate, useSearchParams } from "react-router-dom";

const VisuallyHiddenInput = styled("input")({
  clip: "rect(0 0 0 0)",
  clipPath: "inset(50%)",
  height: 1,
  overflow: "hidden",
  position: "absolute",
  bottom: 0,
  left: 0,
  whiteSpace: "nowrap",
  width: 1,
});

export default function NewPostRoute() {
  const emptyState = {
    title: "",
    fileIds: [],
    body: "",
  };
  const [state, setState] = useLocalStorage<{
    title: string;
    body: string;
    fileIds: string[];
  }>("newPostState", emptyState);
  const [uploadingMedia, setUploadingMedia] = useState(false);
  const api = Api();
  const { userId } = useAuthedState();
  const mediaUploadPollId = useRef<NodeJS.Timeout | undefined>(undefined);
  const [error, setError] = useState<string>("");
  const [info, setInfo] = useState<string>("");

  const [searchParams] = useSearchParams();
  const tradeFor = searchParams.get("tradeFor");
  const { state: locState } = useLocation();
  const navigate = useNavigate();

  const handleChangeState = (e: React.ChangeEvent<HTMLInputElement>) => {
    // limit length of title to 24 characters
    if (e.target.name === "title" && e.target.value.length > 24) {
      return;
    }
    setState({
      ...state,
      [e.target.name]: e.target.value,
    });
  };

  const setupMediaUpPolling = (jobId: string) => {
    // Poll every 5 seconds
    mediaUploadPollId.current = setInterval(async () => {
      try {
        const response = await api.apiJobIdStatusGet({ id: jobId });
        const jobStatus = response.data;

        if (JobStatus.JobFinished === jobStatus) {
          clearInterval(mediaUploadPollId.current);
          setUploadingMedia(false);
          setInfo("Finished compressing files.");
          const response = await api.apiJobIdResultMediaCompressGet({
            id: jobId,
          });
          const jobRes = response.data;

          setState((state) => ({
            ...state,
            fileIds: jobRes.mediaIds,
          }));
        } else if (JobStatus.JobFailed === jobStatus) {
          clearInterval(mediaUploadPollId.current);
          setError("File upload failed.");
          setUploadingMedia(false);
        } else if (JobStatus.JobRunning === jobStatus) {
          setInfo("Compressing files...");
        } else if (JobStatus.JobPending === jobStatus) {
          setInfo("Starting file compression...");
        }
      } catch (e) {
        setError(handleApiErr(e));
      }
    }, 5000);
  };

  useEffect(() => {
    return () => clearInterval(mediaUploadPollId.current);
  }, []);

  const handleFileChange = async (e: ChangeEvent<HTMLInputElement>) => {
    /* setMediaList(e.target.files); // FIXME: merge lists */

    const files = e.target.files;
    if (!files) return;
    setUploadingMedia(true);
    setInfo("Uploading media");
    setError("");
    try {
      const jobId = (await api.apiMediaUploadPost({ uploadMedia: { files } }))
        .data;
      setupMediaUpPolling(jobId);
    } catch (e) {
      setError(handleApiErr(e));
      setUploadingMedia(false);
    }
  };

  const createPost = async (event: any) => {
    event.preventDefault();
    try {
      const response = await api.apiPostsPost({
        newPost: {
          title: state.title,
          media: state.fileIds,
          body: state.body,
        },
      });
      setInfo("Post created successfully");

      const newPostId = response.data;
      setState(emptyState); // the file ids get invalidated by the post request
      event.target.reset();

      if (tradeFor) {
        try {
          await api.apiPostsTradeWithPostForPostPost({
            withPost: newPostId,
            forPost: tradeFor,
          });
        } catch (e) {
          setError(handleApiErr(e));
        }
        setInfo("Traded post successfully");
      }

      if (locState) {
        navigate(locState.path);
      }
    } catch (e) {
      setError(handleApiErr(e));
    }
    /* navigate("/post/" + uuid); */
  };

  const date = getCurrDate();
  const post: ApiPost = {
    tag: "UnhiddenPost",
    contents: {
      post: {
        user: userId,
        title: state.title || "Title",
        body: state.body ? state.body : "Body",
        media: state.fileIds,
        createdAt: date,
        deleted: false,
      },
      usedToUnlock: [],
    },
  };
  return (
    <Grid container spacing={2}>
      <Grid item xs>
        <form onSubmit={createPost}>
          <Stack spacing={2} alignItems="center">
            <TextField
              required
              name="title"
              label="Title"
              variant="standard"
              value={state.title}
              onChange={handleChangeState}
            />
            <Button
              component="label"
              variant="contained"
              tabIndex={-1}
              startIcon={<CloudUploadIcon />}
            >
              Upload media
              <VisuallyHiddenInput
                type="file"
                onChange={handleFileChange}
                multiple
              />
            </Button>
            <Typography>{info}</Typography>
            <Typography color="red">{error}</Typography>
            <TextField
              name="body"
              label="Body"
              value={state.body}
              onChange={handleChangeState}
              multiline
              minRows={3} // Minimum number of rows to display
              maxRows={12} // Maximum number of rows before scrolling
              fullWidth // Makes the input take up the full width of its container
              variant="outlined" // You can use "filled" or "standard" variant too
            />
            <Button variant="contained" type="submit" disabled={uploadingMedia}>
              Create post
            </Button>
          </Stack>
        </form>
      </Grid>
      <Grid item xs sx={{ display: "flex", justifyContent: "center" }}>
        <Box // without the Box the postwidget is fitted to the grid
        >
          <PostWidget
            post={post}
            key={"NewPost"}
            postId={null}
            postActions={{ deletePost: async (_postId) => {} }}
          />
        </Box>
      </Grid>
    </Grid>
  );
}
