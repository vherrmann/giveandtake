import { CardMedia, useTheme } from "@mui/material";
import { SwiperGallery } from "./SwiperGallery";
import { Api } from "../api";
import { useCallback, useEffect, useState } from "react";
import { ErrorWidget, handleApiErr } from "../utils";

export const MediaPostWidget = ({ fileIds }: { fileIds: string[] }) => {
  const theme = useTheme();
  const [files, setFiles] = useState<(File | null)[]>(
    fileIds.map((_: string) => null),
  );
  const [error, setError] = useState<string>("");

  const fetchFiles = useCallback(async () => {
    try {
      const responses = await Promise.all(
        fileIds.map(async (fileId: string) => {
          try {
            const file = Api.apiMediaIdGet({ id: fileId });
            return file;
          } catch (_err) {
            // FIXME: add error handling
            return null;
          }
        }),
      );
      setFiles(responses.map((response) => response?.data ?? null));
    } catch (err) {
      setError("Failed to fetch posts: " + handleApiErr(err));
    }
  }, [fileIds]);

  useEffect(() => {
    fetchFiles();
  }, [fetchFiles]);

  if (files.length === 0) {
    return null;
  }

  return (
    <CardMedia
      sx={{
        ["--swiper-theme-color" as any]: theme.palette.primary.main,
      }}
    >
      <SwiperGallery
        files={files.map((file, i) => ({ id: fileIds[i], file }))}
      />
      <ErrorWidget errors={[error]} />
    </CardMedia>
  );
};
