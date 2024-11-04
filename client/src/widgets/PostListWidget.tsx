import { Box, Stack } from "@mui/material";
import { useEffect, useState } from "react";
import { PostWidget } from "../widgets/PostWidget";
import InfiniteScroll from "react-infinite-scroll-component";
import { useScroll } from "../providers/scroll";
import { Api, WithUUIDApiPost } from "../api";
import { handleApiErr } from "../utils";

export const PostList = ({
  postsFetcher,
}: {
  postsFetcher: () => Promise<WithUUIDApiPost[]>;
}) => {
  const [posts, setPosts] = useState<WithUUIDApiPost[]>([]);
  const [loading, setLoading] = useState<boolean>(true);
  const [error, setError] = useState<string | null>(null);
  const { scrollRef } = useScroll();
  const api = Api();

  const fetchPosts = async () => {
    try {
      const posts = await postsFetcher();
      setPosts(posts);
      setLoading(false);
    } catch (e) {
      // FIXME: more information
      setError("Failed to fetch posts: " + handleApiErr(e));
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchPosts();
  }, []);

  const chunkSize = 4;
  const [shownPosts, setShownPosts] = useState<WithUUIDApiPost[]>([]);

  // FIXME: I don't see a better way to do this
  useEffect(() => {
    setShownPosts(posts.slice(0, chunkSize));
  }, [posts]);

  const addPosts = () => {
    setShownPosts(posts.slice(0, shownPosts.length + chunkSize));
  };

  if (loading) {
    return <p>Loading...</p>;
  }

  if (error !== null) {
    return <p>Error: {error}</p>;
  }
  return (
    <InfiniteScroll
      dataLength={shownPosts.length}
      next={addPosts}
      hasMore={shownPosts.length < posts.length}
      loader={<h4>Loading...</h4>}
      endMessage={<></>} // FIXME: Add rickroll
      refreshFunction={fetchPosts}
      pullDownToRefresh
      pullDownToRefreshThreshold={50}
      pullDownToRefreshContent={
        // FIXME:
        /* <h3 style={{ textAlign: "center" }}>&#8595; Pull down to refresh</h3> */
        <></>
      }
      releaseToRefreshContent={
        // FIXME:
        /* <h3 style={{ textAlign: "center" }}>&#8593; Release to refresh</h3> */
        <></>
      }
      hasChildren={false}
      scrollableTarget={scrollRef.current}
    >
      <Stack
        spacing={2}
        sx={{
          width: "100%",
          maxWidth: 600,
          margin: "auto",
        }}
      >
        {shownPosts.map(({ uuid: postId, value: post }) => {
          if (post) {
            return (
              <Box sx={{ margin: 2 }} key={postId}>
                <PostWidget
                  post={post}
                  postId={postId}
                  postActions={{
                    deletePost: async (postId) => {
                      if (postId) {
                        try {
                          await api.apiPostsIdDelete(postId);
                          setPosts(posts.filter(({ uuid }) => uuid !== postId));
                        } catch (err) {
                          // FIXME: more information
                          setError("Failed to delete post"); // FIXME: use notification
                        }
                      }
                    },
                  }}
                />
              </Box>
            );
          } else {
            return <p>Error: {error}</p>;
          }
        })}
      </Stack>
    </InfiniteScroll>
  );
};
