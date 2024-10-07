import styled from "@emotion/styled";
import { Button, Grid, Stack, TextField } from "@mui/material";
import CloudUploadIcon from "@mui/icons-material/CloudUpload";
import { ChangeEvent, useState } from "react";

// import styles
import { getCurrDate, useLocalStorage } from "../utils";
import { PostWidget } from "../widgets/PostWidget";
import { Api, Post } from "../api";
import { useAuthedState } from "../ProtectedRoute";

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
  const [loadingMedia, setLoadingMedia] = useState(false);
  const api = new Api();
  const { userId } = useAuthedState();

  const handleChangeState = (e: React.ChangeEvent<HTMLInputElement>) => {
    setState({
      ...state,
      [e.target.name]: e.target.value,
    });
  };

  const handleFileChange = async (e: ChangeEvent<HTMLInputElement>) => {
    /* setMediaList(e.target.files); // FIXME: merge lists */

    const files = e.target.files;
    if (!files) return;
    // Inform user of upload progress
    setLoadingMedia(true);
    try {
      const response = await api.apiMediaUploadPostR({
        body: files,
      });

      // update state with the new media ids
      setState((state) => ({
        ...state,
        fileIds: response.mediaIds,
      }));
    } catch (err) {
      // FIXME: more information
      // FIXME: inform user
      console.error(err);
    }
    setLoadingMedia(false);
  };

  const createPost = async (event: any) => {
    event.preventDefault();
    const uuid = await api.apiPostsPost({
      newPost: {
        title: state.title,
        media: state.fileIds,
        body: state.body,
      },
    });

    setState(emptyState); // the file ids get invalidated by the post request
    event.target.reset();
    /* navigate("/post/" + uuid); */
  };

  const date = getCurrDate();
  const post: Post = {
    user: userId, // FIXME
    title: state.title || "Title",
    body: state.body ? state.body : "Body",
    media: state.fileIds,
    createdAt: date,
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
            {loadingMedia && <i>Sending Media</i>}
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
            <Button variant="contained" type="submit" disabled={loadingMedia}>
              Create post
            </Button>
          </Stack>
        </form>
      </Grid>
      <Grid item xs sx={{ display: "flex", justifyContent: "center" }}>
        <PostWidget
          post={post}
          key={"NewPost"}
          postId={null}
          postActions={{ deletePost: async (_postId) => {} }}
        />
      </Grid>
    </Grid>
  );
}
