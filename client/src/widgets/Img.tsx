import { ComponentProps, useState } from "react";

type ImgParams = {
  media: Blob | MediaSource;
  cover?: boolean;
} & ComponentProps<"img">;

export const Img = ({ media, cover, ...props }: ImgParams) => {
  const [isLoaded, setIsLoaded] = useState(false);
  return (
    <img
      {...props}
      src={URL.createObjectURL(media)}
      alt={props.alt || ""}
      style={{
        maxHeight: "100%",
        maxWidth: "100%",
        objectFit: cover ? "cover" : "scale-down",
        height: cover ? "100%" : "auto",
        width: cover ? "100%" : "auto",
        visibility: isLoaded ? undefined : "hidden",
        ...props.style,
      }}
      onLoad={() => setIsLoaded(true)}
    />
  );
};
