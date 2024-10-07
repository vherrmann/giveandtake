import { useNavigate, useParams } from "react-router";
import { PostWidget } from "../widgets/PostWidget";
import { useEffect, useState } from "react";
import { Api, Post } from "../api";

export default function PostRoute() {
  const { postId } = useParams<{ postId: string }>();
  const [post, setPost] = useState<Post | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState<boolean>(true);
  const navigate = useNavigate();
  const api = new Api();

  const fetchPosts = async () => {
    if (!postId) {
      throw new Error("No postId provided for useParams");
    }
    try {
      const post = await api.apiPostsIdGet({ id: postId });
      setPost(post);
      setLoading(false);
    } catch (err) {
      setError("Failed to fetch posts");
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchPosts();
  }, [postId]);

  if (loading) {
    return <p>Loading...</p>;
  }

  if (!postId || error) {
    return <p>Error: {error || (!postId && "post uuid missing")}</p>;
  }

  return (
    post && (
      <PostWidget
        post={post}
        postId={postId}
        key={postId}
        postActions={{
          deletePost: async (postId) => {
            if (postId) {
              try {
                await api.apiPostsIdDelete({ id: postId });
                navigate("/");
              } catch (err) {
                setError("Failed to delete post"); // FIXME: use notification
              }
            }
          },
        }}
      />
    )
  );
}
