import { Box, IconButton, Tooltip } from "@mui/material";
import { useAuthState } from "../providers/auth";
import { useLocation } from "react-router";
import * as router from "react-router";

import { Api } from "../api";
import { useEffect, useState } from "react";
import RssFeedIcon from "@mui/icons-material/RssFeed";
import CheckIcon from "@mui/icons-material/Check";

export const FeedWidget = () => {
  const { isAuthenticated } = useAuthState();
  const [fUrl, setFUrl] = useState<string | null>("test");
  const [replaced, setReplaced] = useState<boolean>(false);
  const location = useLocation();
  const api = Api();

  const locToFUrl = (location: router.Location<any>) => {
    if (location.pathname === "/") {
      return api.apiFeedUrlPost({ body: "MainFeed" });
    } else {
      return null;
    }
  };

  const fUrlPromise = locToFUrl(location);

  useEffect(() => {
    const fetchUrl = async () => {
      if (!fUrlPromise) {
        return;
      }
      try {
        const response = await fUrlPromise;
        setFUrl(response.data.feedUrl);
      } catch (e) {
        // FIXME
        console.log(e);
      }
    };
    fetchUrl();
  }, []);

  if (!isAuthenticated) {
    return <></>;
  }

  if (!fUrlPromise) {
    return <></>;
  }
  const handleClick = async () => {
    if (fUrl === null) {
      return;
    }
    try {
      await navigator.clipboard.writeText(fUrl);
    } catch (err) {
      console.error("Failed to copy: ", err);
    }
    setReplaced(true);
    setTimeout(() => {
      setReplaced(false);
    }, 2000);
  };
  return (
    <Tooltip
      title={
        !fUrl
          ? "Loading..."
          : replaced
            ? "Copied"
            : "Copy feed url to clipboard"
      }
    >
      <Box // Mui wants this
      >
        <IconButton onClick={handleClick} disabled={fUrl === null}>
          {replaced ? <CheckIcon /> : <RssFeedIcon />}
        </IconButton>
      </Box>
    </Tooltip>
  );
};
