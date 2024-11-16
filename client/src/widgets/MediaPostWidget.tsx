import { CardMedia, useTheme } from "@mui/material";
import { SwiperGallery } from "./SwiperGallery";
import { Api } from "../api";
import { useCallback, useEffect, useState } from "react";
import { ApiResultState, handleApiErr } from "../utils";

export const MediaPostWidget = ({ fileIds }: { fileIds: string[] }) => {
  type FileResType = { id: string; apiResState: ApiResultState<File> };
  const theme = useTheme();
  const [files, setFiles] = useState<FileResType[]>(
    fileIds.map((fileId: string) => ({
      id: fileId,
      apiResState: { loading: true, error: "", data: null },
    })),
  );
  const fetchFile = useCallback(async (i: number, fileId: string) => {
    const setFile = (file: ApiResultState<File>) => {
      setFiles((prevFiles) => {
        const newFiles = [...prevFiles];
        newFiles[i] = { id: fileId, apiResState: file };
        return newFiles;
      });
    };
    setFile({ loading: true, error: "", data: null });
    try {
      const response = await Api.apiMediaIdGet({ id: fileId });
      setFile({ loading: false, error: "", data: response.data });
    } catch (e) {
      setFile({ loading: false, error: handleApiErr(e), data: null });
    }
  }, []);

  useEffect(() => {
    fileIds.forEach((fileId: string, i: number, _) => {
      fetchFile(i, fileId);
    });
  }, [fileIds, fetchFile]);

  if (files.length === 0) {
    return null;
  }

  // FIXME: cancel download when changing location?

  return (
    <CardMedia
      sx={{
        ["--swiper-theme-color" as any]: theme.palette.primary.main,
      }}
    >
      <SwiperGallery files={files} />
    </CardMedia>
  );
};
