import { useNavigate, useParams } from "react-router";
import { PostWidget } from "../widgets/PostWidget";
import {
  apiPostToTitle,
  DApi,
  ErrorWidget,
  useApiState,
  useTitle,
} from "../utils";

export default function PostRoute() {
  const { postId } = useParams<{ postId: string }>();
  const navigate = useNavigate();

  if (!postId) {
    throw new Error("No postId provided for useParams");
  }
  const [errorP, post, { loading, refetch }] = useApiState(DApi.apiPostsIdGet, {
    id: postId,
  });
  useTitle(`Post ${post && apiPostToTitle(post)}`);

  return (
    <>
      <ErrorWidget errors={[errorP]} />
      {post ? (
        <PostWidget
          post={post}
          postId={postId}
          key={postId}
          onDelete={() => navigate("/")}
          refetch={refetch}
        />
      ) : loading ? (
        "Loading..."
      ) : (
        ""
      )}
    </>
  );
}
