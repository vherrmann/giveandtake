import { ReactNode, useEffect, useRef, useState } from "react";

import * as swiper from "swiper";
import { Navigation, Pagination, Keyboard, Zoom } from "swiper/modules";
import "swiper/css";
import "swiper/css/navigation";
import "swiper/css/pagination";
import "swiper/css/zoom";

import "./swiper.css";
import {
  Backdrop,
  Box,
  LinearProgress,
  Modal,
  Fade,
  Toolbar,
  useTheme,
  IconButton,
  ThemeProvider,
  Theme,
  createTheme,
} from "@mui/material";
import {
  Swiper,
  SwiperRef,
  SwiperSlide,
  useSwiper,
  useSwiperSlide,
} from "swiper/react";
import ReactPlayer, { ReactPlayerProps } from "react-player";
import CloseIcon from "@mui/icons-material/Close";
import "./react-player.css"; /* https://stackoverflow.com/questions/26979003/space-after-html5s-video-tag */

const UncontrolledVideoPlayer = ({
  file,
  ...props
}: { file: File } & ReactPlayerProps) => {
  // createObjectURL is not determintistic and assigns a new url on every rerender, we therefore
  // use useState to save the url from the first render
  const [localFileUrl, _] = useState(URL.createObjectURL(file));
  const [progress, setProgress] = useState(0);
  return (
    <Box
      sx={{
        position: "relative",
      }}
    >
      <ReactPlayer
        url={localFileUrl}
        className="ReactPlayer" /* https://stackoverflow.com/questions/26979003/space-after-html5s-video-tag */
        loop={true}
        volume={0}
        muted={true}
        playsinline={true}
        onProgress={(state) => {
          setProgress(state.played);
        }}
        progressInterval={100}
        width="100%"
        height="100%"
        {...props}
      />
      <LinearProgress
        variant="determinate"
        value={progress * 100}
        sx={{
          position: "absolute",
          bottom: 0,
          left: 0,
          width: "100%",
          zIndex: 1,
        }}
      />
    </Box>
  );
};

export const SwiperGallery = ({
  files,
}: {
  files: { id: string; file: File | null }[];
}) => {
  const paginationRef = useRef<HTMLDivElement>(null);
  const paginationFSRef = useRef<HTMLDivElement>(null);
  const [swiperFSOpen, setSwiperFSOpen] = useState(false);
  const curSlide = useRef<number>(0);
  const theme = useTheme();
  const swiperRef = useRef<SwiperRef>(null);
  const swiperFSRef = useRef<SwiperRef>(null);

  const slides = (fullscreenp: boolean) => {
    return files
      .map(({ file }) => {
        if (!file) {
          return "Loading...";
        }
        const localFileUrl = URL.createObjectURL(file);
        const fileType = file.type; // Get the MIME type
        if (fileType.startsWith("image/")) {
          return (
            <img
              src={URL.createObjectURL(file)}
              style={{
                objectFit: fullscreenp ? "scale-down" : "cover",
                height: fullscreenp ? "auto" : "100%",
                width: fullscreenp ? "auto" : "100%",
              }}
            />
          );
        } else if (fileType.startsWith("video/")) {
          return fullscreenp ? (
            <ReactPlayer
              url={localFileUrl}
              className="ReactPlayer"
              controls
              width="auto"
              height="auto"
            />
          ) : (
            <UncontrolledVideoPlayer file={file} playing={!swiperFSOpen} />
          );
        } else {
          console.log("This is neither an image nor a video."); // FIXME: throw error, remove from list (remove at addition)
        }
      })
      .map((slide, i) => (
        <SwiperSlide
          key={files[i].id}
          style={{
            backgroundColor: "inherit",
          }}
        >
          {fullscreenp ? (
            <Box className="swiper-zoom-container">{slide}</Box>
          ) : (
            slide
          )}
        </SwiperSlide>
      ));
  };

  const closeModal = () => {
    if (swiperRef.current?.swiper && !swiperRef.current.swiper.destroyed) {
      swiperRef.current.swiper.slideTo(curSlide.current);
    }
    setSwiperFSOpen(false);
  };

  const openModal = () => {
    if (swiperFSRef.current?.swiper && !swiperFSRef.current.swiper.destroyed) {
      swiperFSRef.current.swiper.slideTo(curSlide.current);
    }
    setSwiperFSOpen(true);
  };

  return (
    <>
      <Swiper
        ref={swiperRef}
        spaceBetween={10}
        navigation
        centeredSlides
        pagination={{
          clickable: true,
          el: paginationRef.current,
        }}
        keyboard={{ enabled: true }}
        modules={[Navigation, Pagination, Keyboard]}
        style={{
          aspectRatio: 1, //square
          userSelect: "none",
        }}
        onClick={(_s, _e) => openModal()}
        onSlideChange={(s) => {
          curSlide.current = s.realIndex;
        }}
        initialSlide={curSlide.current}
      >
        {slides(false)}
      </Swiper>
      <Box
        ref={paginationRef}
        style={{
          display: "flex",
          justifyContent: "center",
          alignItems: "center",
          marginTop: "4px",
        }}
      ></Box>
      <Modal
        open={swiperFSOpen}
        onClose={closeModal}
        closeAfterTransition
        keepMounted
        slotProps={{
          backdrop: {
            timeout: 500,
          },
        }}
        sx={{
          ["--swiper-theme-color" as any]: theme.palette.primary.main,
          backgroundColor: "black",
        }}
      >
        {
          // FIXME: Zoom in from the clicked image/video button
        }
        <Fade in={swiperFSOpen}>
          <Box
            sx={{
              position: "fixed",
              top: 0,
              left: 0,
              width: "100vw",
              height: "100vh",
              display: "flex",
              flexDirection: "column",
            }}
          >
            <Toolbar>
              <Box
                ref={paginationFSRef}
                sx={{
                  ["--swiper-pagination-fraction-color" as any]:
                    theme.palette.primary.main,
                }}
              />
              <Box sx={{ flexGrow: 1 }} />
              <IconButton onClick={closeModal}>
                <CloseIcon color="primary" />
              </IconButton>
            </Toolbar>
            <Swiper
              ref={swiperFSRef}
              spaceBetween={10}
              navigation
              zoom
              pagination={{
                clickable: true,
                type: "fraction",
                el: paginationFSRef.current,
              }}
              keyboard={{ enabled: true }}
              modules={[Navigation, Pagination, Keyboard, Zoom]}
              style={{
                userSelect: "none",
              }}
              onSlideChange={(s) => {
                curSlide.current = s.realIndex;
              }}
              initialSlide={curSlide.current}
              onClick={(s, event) => {
                // close modal on click outside of media
                const target = event.target as HTMLElement | null;
                const mediaEls = ["VIDEO", "IMG", "PICTURE", "CANVAS", "AUDIO"];
                if (target && !mediaEls.includes(target.tagName)) {
                  closeModal();
                }
              }}
            >
              {slides(true)}
            </Swiper>
          </Box>
        </Fade>
      </Modal>
    </>
  );
};

// media count
// zoom icon
// close icon
