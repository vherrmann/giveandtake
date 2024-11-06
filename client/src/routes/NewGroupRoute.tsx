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

export default function NewGroupRoute() {
  const emptyState = {
    name: "",
  };
  const [state, setState] = useLocalStorage<{
    name: string;
  }>("newPostState", emptyState);
  const api = Api();
  const { userId } = useAuthedState();
  const [error, setError] = useState<string>("");

  const navigate = useNavigate();

  const handleChangeState = (e: React.ChangeEvent<HTMLInputElement>) => {
    // limit length of title to 20 characters
    if (e.target.name === "name" && e.target.value.length > 20) {
      return;
    }
    setState({
      ...state,
      [e.target.name]: e.target.value,
    });
  };

  const createPost = async (event: any) => {
    event.preventDefault();
    try {
      const response = await api.apiGroupsPost({
        newGroup: {
          name: state.name,
        },
      });
      const uuid = response.data;

      const newPostId = response.data;
      setState(emptyState); // the file ids get invalidated by the post request
      event.target.reset();
      navigate("/group/" + uuid);
    } catch (e) {
      setError(handleApiErr(e));
    }
  };

  return (
    <form onSubmit={createPost}>
      <Stack spacing={2} alignItems="center">
        <TextField
          required
          name="name"
          label="Name"
          variant="standard"
          value={state.name}
          onChange={handleChangeState}
        />
        <Typography color="red">{error}</Typography>
        <Button variant="contained" type="submit">
          Create group
        </Button>
      </Stack>
    </form>
  );
}
