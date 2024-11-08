import { CardMedia, useTheme } from "@mui/material";
import { SwiperSlide } from "swiper/react";
import ReactPlayer from "react-player";
import { SwiperGallery } from "./SwiperGallery";
import { Api } from "../api";
import { useEffect, useState } from "react";

export const MediaPostWidget = ({ fileIds }: { fileIds: string[] }) => {
  const api = Api();
  const theme = useTheme();
  const [files, setFiles] = useState<(File | null)[]>(
    fileIds.map((_: string) => null),
  );
  const [error, setError] = useState<string>("");

  const fetchFiles = async () => {
    try {
      const responses = await Promise.all(
        fileIds.map(async (fileId: string) => {
          try {
            const file = api.apiMediaIdGet({ id: fileId });
            return file;
          } catch (err) {
            // FIXME: add error handling
            return null;
          }
        }),
      );
      setFiles(responses.map((response) => response?.data ?? null));
    } catch (err) {
      setError("Failed to fetch posts");
    }
  };

  useEffect(() => {
    fetchFiles();
  }, [fileIds]);

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
    </CardMedia>
  );
};
