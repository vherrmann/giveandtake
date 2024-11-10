import { ReactNode } from "react";
import { WithUUIDUserPublic } from "../api";
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
import { DApi, ErrorWidget, useApi, useApiState } from "../utils";

export const FriendsRequestWidget = () => {
  const [errorFR, friendReqs, { refetch }] = useApiState(
    DApi.apiFriendsRequestGet,
  );

  const friendReqsTo = friendReqs?.requestsToYou;
  const friendReqsFrom = friendReqs?.requestsFromYou;

  const noneCurr = (reqs: WithUUIDUserPublic[] | undefined, el: ReactNode) => {
    return reqs?.length === 0 ? (
      <Typography align="center">None currently</Typography>
    ) : (
      el
    );
  };

  const [errorAR, acceptRequest] = useApi(
    DApi.apiFriendsRequestFriendIdAcceptPost,
    { onSuccess: refetch },
  );

  const [errorRR, rejectRequest] = useApi(
    DApi.apiFriendsRequestFriendIdRejectPost,
    { onSuccess: refetch },
  );

  const [errorCR, cancelRequest] = useApi(
    DApi.apiFriendsRequestFriendIdCancelPost,
    { onSuccess: refetch },
  );

  return (
    <StandardCard>
      <CardHeader
        title={
          <Typography gutterBottom variant="h5" align="center">
            Friend requests
          </Typography>
        }
      />

      <ErrorWidget errors={[errorFR, errorAR, errorRR, errorCR]} />
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
                      onClick={() => acceptRequest({ friendId })}
                    >
                      <DoneIcon />
                    </IconButton>
                    <IconButton
                      edge="end"
                      aria-label="reject-request"
                      onClick={() => rejectRequest({ friendId })}
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
              <ListUserItem
                userId={friendId}
                userPublic={friend}
                secondaryAction={
                  <Box sx={{ display: "flex", gap: 1 }}>
                    <IconButton
                      edge="end"
                      aria-label="cancel-request"
                      onClick={() => cancelRequest({ friendId })}
                    >
                      <ClearIcon />
                    </IconButton>
                  </Box>
                }
              />
            )) ?? "Loading..."}
          </List>,
        )}
      </CardContent>
    </StandardCard>
  );
};
