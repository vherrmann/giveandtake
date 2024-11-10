import { useNavigate, useParams } from "react-router";
import { PostWidget } from "../widgets/PostWidget";
import { DApi, ErrorWidget, useApiState } from "../utils";

export default function PostRoute() {
  const { postId } = useParams<{ postId: string }>();
  const navigate = useNavigate();

  if (!postId) {
    throw new Error("No postId provided for useParams");
  }
  const [errorP, post, { loading }] = useApiState(DApi.apiPostsIdGet, {
    id: postId,
  });

  return (
    <>
      <ErrorWidget errors={[errorP]} />
      {post && !loading ? (
        <PostWidget
          post={post}
          postId={postId}
          key={postId}
          onDelete={() => navigate("/")}
        />
      ) : (
        "Loading..."
      )}
    </>
  );
}
