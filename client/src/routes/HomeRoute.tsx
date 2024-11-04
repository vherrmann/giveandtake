import { Stack } from "@mui/material";
import { Api } from "../api";
import { PostList } from "../widgets/PostListWidget";

export const HomeRoute = () => {
  const api = Api();
  return (
    <Stack spacing={2} alignItems="center">
      <PostList
        postsFetcher={async () =>
          // FIXME: add error handling
          (await api.apiPostsFeedGet()).data
        }
      />
    </Stack>
  );
};

export default HomeRoute;
