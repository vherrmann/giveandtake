import {
  Box,
  Button,
  CardContent,
  CardHeader,
  IconButton,
  Paper,
  Stack,
  Tab,
  Tabs,
  TextField,
  Typography,
} from "@mui/material";
import CloudUploadIcon from "@mui/icons-material/CloudUpload";
import AddCircleOutlineIcon from "@mui/icons-material/AddCircleOutline";
import DeleteIcon from "@mui/icons-material/Delete";
import ArrowBackIcon from "@mui/icons-material/ArrowBack";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import { ChangeEvent, ReactNode, useCallback, useState } from "react";

// import styles
import {
  arraySwap,
  ErrorWidget,
  getCurrDate,
  handleApiErr,
  useLocalStorage,
  useMediaUploadJob,
  useTitle,
  withApi,
} from "../utils";
import { PostWidget } from "../widgets/PostWidget";
import { Api, ApiPost } from "../api";
import { useAuthedState } from "../ProtectedRoute";
import { useLocation, useNavigate, useSearchParams } from "react-router-dom";
import { VisuallyHiddenInput } from "../widgets/VisuallyHiddenInput";
import { StandardCard } from "../widgets/StandardCard";
import { MediaPostWidget } from "../widgets/MediaPostWidget";

const useless = () => {};

// https://mui.com/material-ui/react-tabs/
interface TabPanelProps {
  children?: ReactNode;
  index: number;
  activeTab: number;
}
function CustomTabPanel(props: TabPanelProps) {
  const { children, activeTab: value, index, ...other } = props;

  return (
    <div
      role="tabpanel"
      hidden={value !== index}
      id={`simple-tabpanel-${index}`}
      {...other}
    >
      {value === index && <Box sx={{ p: 3 }}>{children}</Box>}
    </div>
  );
}

const UploadMediaWidget = ({
  fileIds,
  setFileIds,
  setUploadingLoadingParent,
}: {
  fileIds: string[];
  setFileIds: (fn: (ids: string[]) => string[]) => void;
  setUploadingLoadingParent: (loading: boolean) => void;
}) => {
  const [uploadingMedia, setUploadingMedia_] = useState(false);
  const setUploadingMedia = useCallback(
    (x: boolean) => {
      setUploadingLoadingParent(x);
      setUploadingMedia_(x);
    },
    [setUploadingMedia_, setUploadingLoadingParent],
  );
  const [info, setInfo] = useState<string>("");
  const [error, setError] = useState<string>("");

  const onJobFinished = useCallback(
    async (jobId: string) => {
      setUploadingMedia(false);
      withApi(async (api) => {
        const response = await api.apiJobIdResultMediaCompressGet({
          id: jobId,
        });
        const jobRes = response.data;

        setFileIds((fileIds) => [...fileIds, ...jobRes.mediaIds]);
      }, setError);
    },
    [setUploadingMedia, setFileIds],
  );
  const onJobFailed = useCallback(() => {
    setUploadingMedia(false);
  }, [setUploadingMedia]);

  const setupMediaUpPolling = useMediaUploadJob({
    setInfo,
    setError,
    onJobFinished,
    onJobFailed,
  });

  const handleFileChange = useCallback(
    async (e: ChangeEvent<HTMLInputElement>) => {
      /* setMediaList(e.target.files); // FIXME: merge lists? */

      const files = e.target.files;
      if (!files) return;
      setUploadingMedia(true);
      setInfo("Uploading media");
      setError("");
      try {
        const jobId = (await Api.apiMediaUploadPost({ uploadMedia: { files } }))
          .data;
        setupMediaUpPolling(jobId);
      } catch (e) {
        setError(handleApiErr(e));
        setUploadingMedia(false);
      }
    },
    [setUploadingMedia, setInfo, setError, setupMediaUpPolling],
  );

  const mediaActions = useCallback(
    (i: number, _fullscreenp: boolean, slideTo: (index: number) => void) => {
      const id = fileIds[i];
      const mediaActionButtonSX = {
        backgroundColor: "rgba(0,0,0,0.5)",
        color: "white",
        "&:hover": { backgroundColor: "rgba(0,0,0,0.8)" },
      };

      return (
        <>
          <label htmlFor="add-file-icon-button">
            <VisuallyHiddenInput
              type="file"
              onChange={handleFileChange}
              multiple
              id="add-file-icon-button"
            />
            <IconButton
              sx={{
                ...mediaActionButtonSX,
                position: "absolute",
                bottom: 10,
                right: 10,
              }}
              disabled={uploadingMedia}
              component="span"
            >
              <AddCircleOutlineIcon />
            </IconButton>
          </label>
          <Box
            sx={{
              position: "absolute",
              top: 10,
              right: 10,
            }}
          >
            <IconButton
              sx={{
                ...mediaActionButtonSX,
              }}
              disabled={i === 0}
              onClick={() => {
                setFileIds((fileIds) => arraySwap(fileIds, i, i - 1));
                slideTo(i - 1);
              }}
            >
              <ArrowBackIcon />
            </IconButton>
            <IconButton
              sx={{
                ...mediaActionButtonSX,
              }}
              disabled={i === fileIds.length - 1}
              onClick={() => {
                setFileIds((fileIds) => arraySwap(fileIds, i, i + 1));
                slideTo(i + 1);
              }}
            >
              <ArrowForwardIcon />
            </IconButton>
            <IconButton
              sx={{
                ...mediaActionButtonSX,
              }}
              onClick={
                () => {
                  setFileIds((fileIds) => fileIds.filter((x) => x !== id));
                } // FIXME: remove draft media from server
              }
            >
              <DeleteIcon />
            </IconButton>
          </Box>
        </>
      );
    },
    [fileIds, setFileIds, uploadingMedia, handleFileChange],
  );

  return (
    <>
      {fileIds.length === 0 ? (
        <Button
          component="label"
          variant="contained"
          tabIndex={-1}
          startIcon={<CloudUploadIcon />}
          disabled={uploadingMedia}
        >
          Upload media
          <VisuallyHiddenInput
            type="file"
            onChange={handleFileChange}
            multiple
          />
        </Button>
      ) : (
        <Box sx={{ width: "100%" }}>
          <MediaPostWidget fileIds={fileIds} actions={mediaActions} />
        </Box>
      )}

      <Typography>{info}</Typography>
      <ErrorWidget errors={[error]} />
    </>
  );
};

export default function NewPostRoute() {
  useTitle("Create new post");
  const emptyState = {
    title: "",
    fileIds: [],
    body: "",
  };
  const [state, setState] = useLocalStorage<{
    title: string;
    body: string;
    fileIds: string[];
  }>("newPostState", emptyState);
  const { userId } = useAuthedState();
  const [error, setError] = useState<string>("");
  const [info, setInfo] = useState<string>("");
  const [activeTab, setActiveTab] = useState(0);
  const [uploadingMedia, setUploadingMedia] = useState(false);

  const [searchParams] = useSearchParams();
  const tradeFor = searchParams.get("tradeFor");
  const { state: locState } = useLocation();
  const navigate = useNavigate();

  const handleChangeState = (e: React.ChangeEvent<HTMLInputElement>) => {
    // limit length of title to 24 characters
    if (e.target.name === "title" && e.target.value.length > 24) {
      return;
    }
    setState({
      ...state,
      [e.target.name]: e.target.value,
    });
  };

  const createPost = async (event: any) => {
    event.preventDefault();
    try {
      const response = await Api.apiPostsPost({
        newPost: {
          title: state.title,
          media: state.fileIds,
          body: state.body,
        },
      });
      setInfo("Post created successfully");

      const newPostId = response.data;
      setState(emptyState); // the file ids get invalidated by the post request
      event.target.reset();

      if (tradeFor) {
        try {
          await Api.apiPostsTradeWithPostForPostPost({
            withPost: newPostId,
            forPost: tradeFor,
          });
        } catch (e) {
          setError(handleApiErr(e));
        }
        setInfo("Traded post successfully");
      }

      if (locState) {
        navigate(locState.path);
      }
    } catch (e) {
      setError(handleApiErr(e));
    }
    /* navigate("/post/" + uuid); */
  };

  const date = getCurrDate();
  const post: ApiPost = {
    tag: "UnhiddenPost",
    contents: {
      post: {
        post: {
          user: userId,
          title: state.title || "Title",
          body: state.body ? state.body : "Body",
          media: state.fileIds,
          createdAt: date,
          deleted: false,
        },
        liked: false,
      },
      usedToUnlock: [],
    },
  };

  return (
    <Paper>
      <Box sx={{ borderBottom: 1, borderColor: "divider" }}>
        <Tabs
          value={activeTab}
          onChange={(_e, i) => setActiveTab(i)}
          variant="fullWidth"
        >
          <Tab label="Input" />
          <Tab label="Preview" />
        </Tabs>
      </Box>
      <CustomTabPanel activeTab={activeTab} index={0}>
        <StandardCard variant="outlined">
          <CardHeader
            title={
              <Typography variant="h5" align="center">
                Create new post
              </Typography>
            }
          />
          <CardContent>
            <form onSubmit={createPost}>
              <Stack spacing={2} alignItems="center">
                <TextField
                  required
                  name="title"
                  label="Title"
                  variant="standard"
                  value={state.title}
                  onChange={handleChangeState}
                />
                <UploadMediaWidget
                  fileIds={state.fileIds}
                  setFileIds={useCallback(
                    (fn: (fileIds: string[]) => string[]) =>
                      setState((state) => ({
                        ...state,
                        fileIds: fn(state.fileIds),
                      })),
                    [setState],
                  )}
                  setUploadingLoadingParent={setUploadingMedia}
                />
                <TextField
                  name="body"
                  label="Body"
                  value={state.body}
                  onChange={handleChangeState}
                  multiline
                  minRows={3} // Minimum number of rows to display
                  maxRows={12} // Maximum number of rows before scrolling
                  fullWidth // Makes the input take up the full width of its container
                  variant="outlined" // You can use "filled" or "standard" variant too
                />
                <Button
                  variant="contained"
                  type="submit"
                  disabled={uploadingMedia}
                >
                  Create post
                </Button>
                <Typography>{info}</Typography>
                <ErrorWidget errors={[error]} />
              </Stack>
            </form>
          </CardContent>
        </StandardCard>
      </CustomTabPanel>
      <CustomTabPanel activeTab={activeTab} index={1}>
        <Box // without the Box the postwidget is fitted to the grid
        >
          <PostWidget
            post={post}
            key={"NewPost"}
            postId={null}
            refetch={useless}
            cardProps={{ variant: "outlined" }}
          />
        </Box>
      </CustomTabPanel>
    </Paper>
  );
}
