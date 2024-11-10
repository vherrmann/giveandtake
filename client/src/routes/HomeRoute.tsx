import { Stack } from "@mui/material";
import { Api } from "../api";
import { PostList } from "../widgets/PostListWidget";
import { useCallback } from "react";

export const HomeRoute = () => {
  const postsFetcher = useCallback(
    async () => (await Api.apiPostsFeedGet()).data,
    [],
  );
  return (
    <Stack spacing={2} alignItems="center">
      <PostList postsFetcher={postsFetcher} />
    </Stack>
  );
};

export default HomeRoute;
