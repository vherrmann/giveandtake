import { Button, Stack, TextField, Typography } from "@mui/material";
import { useState } from "react";

// import styles
import { handleApiErr, useTitle } from "../utils";
import { Api } from "../api";
import { useNavigate } from "react-router-dom";

export default function NewGroupRoute() {
  useTitle("Create new group");
  const emptyState = {
    name: "",
  };
  const [state, setState] = useState<{
    name: string;
  }>(emptyState);

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
      const response = await Api.apiGroupsPost({
        newGroup: {
          name: state.name,
        },
      });
      const uuid = response.data;

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
