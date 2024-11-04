import { ReactNode, useEffect, useState } from "react";
import { Api, FriendsRequestGetResponse, WithUUIDUserPublic } from "../api";
import {
  Box,
  CardContent,
  CardHeader,
  Divider,
  IconButton,
  List,
  Typography,
} from "@mui/material";
import DoneIcon from "@mui/icons-material/Done";
import ClearIcon from "@mui/icons-material/Clear";
import { StandardCard } from "./StandardCard";
import { ListUserItem } from "./ListUserItem";

export const FriendsRequestWidget = ({ userId }: { userId: string }) => {
  const [friendReqs, setFriendReqs] =
    useState<FriendsRequestGetResponse | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  const api = Api();
  const fetchFriendRequests = async () => {
    try {
      setLoading(true);
      const response = await api.apiFriendsRequestGet();
      setFriendReqs(response.data);
      setLoading(false);
    } catch (err) {
      // FIXME: more detailed information
      setError("Failed to fetch posts");
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchFriendRequests();
  }, []);

  const friendReqsTo = friendReqs?.requestsToYou;
  const friendReqsFrom = friendReqs?.requestsFromYou;

  // return nothing if requests empty
  /* if (friendReqsTo?.length === 0 && friendReqsFrom?.length === 0) {
   *   return <></>;
   * } */
  const noneCurr = (reqs: WithUUIDUserPublic[] | undefined, el: ReactNode) => {
    return reqs?.length === 0 ? (
      <Typography align="center">None currently</Typography>
    ) : (
      el
    );
  };

  const acceptRequest = (friendId: string) => async () => {
    try {
      await api.apiFriendsRequestFriendIdAcceptPost(friendId);
      fetchFriendRequests();
      // FIXME: update friend list
    } catch (err) {
      // FIXME: inform user
    }
  };

  const rejectRequest = (friendId: string) => async () => {
    try {
      await api.apiFriendsRequestFriendIdRejectPost(friendId);
      fetchFriendRequests();
    } catch (err) {
      // FIXME: inform user
    }
  };

  return (
    <StandardCard>
      <CardHeader
        title={
          <Typography gutterBottom variant="h5" align="center">
            Friendrequests
          </Typography>
        }
      />

      <Divider />
      <CardContent>
        <Typography gutterBottom variant="h6" align="center">
          To you
        </Typography>
        {noneCurr(
          friendReqsTo,
          <List>
            {friendReqsTo?.map(({ key: friendId, value: friend }) => (
              <ListUserItem
                userId={friendId}
                userPublic={friend}
                key={friendId}
                secondaryAction={
                  <Box sx={{ display: "flex", gap: 1 }}>
                    <IconButton
                      edge="end"
                      aria-label="accept-request"
                      onClick={acceptRequest(friendId)}
                    >
                      <DoneIcon />
                    </IconButton>
                    <IconButton
                      edge="end"
                      aria-label="reject-request"
                      onClick={rejectRequest(friendId)}
                    >
                      <ClearIcon />
                    </IconButton>
                  </Box>
                }
              />
            ))}
          </List>,
        )}
      </CardContent>
      <Divider />
      <CardContent>
        <Typography gutterBottom variant="h6" align="center">
          From you
        </Typography>
        {noneCurr(
          friendReqsFrom,
          <List>
            {friendReqsFrom?.map(({ key: friendId, value: friend }) => (
              <ListUserItem userId={friendId} userPublic={friend} />
            ))}
          </List>,
        )}
      </CardContent>
    </StandardCard>
  );
};
