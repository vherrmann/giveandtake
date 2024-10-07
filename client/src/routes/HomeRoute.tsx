import { Stack } from "@mui/material";
import { Api } from "../api";
import { PostList } from "../widgets/PostListWidget";

export const HomeRoute = () => {
  const api = new Api();
  return (
    <Stack spacing={2} alignItems="center">
      <PostList postsFetcher={async () => await api.apiPostsFeedGet()} />
    </Stack>
  );
};

export default HomeRoute;
