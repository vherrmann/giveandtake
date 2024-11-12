import { Box, IconButton, Tooltip } from "@mui/material";
import { useLocation } from "react-router";

import { useState } from "react";
import RssFeedIcon from "@mui/icons-material/RssFeed";
import CheckIcon from "@mui/icons-material/Check";
import { DApi, ErrorWidget, useApiState } from "../utils";

export const FeedWidget = () => {
  const [replaced, setReplaced] = useState<boolean>(false);
  const location = useLocation();
  const [errorFU, furlResponse] = useApiState<"apiFeedUrlPost">(
    DApi.apiFeedUrlPost,
    (location.pathname === "/" && { body: "MainFeed" }) || null,
  );
  const fUrl = furlResponse?.feedUrl;

  if (!fUrl) {
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
        <ErrorWidget errors={[errorFU]} />
      </Box>
    </Tooltip>
  );
};
