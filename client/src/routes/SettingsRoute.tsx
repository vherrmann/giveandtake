import {
  Button,
  CardContent,
  CardHeader,
  Stack,
  TextField,
  Typography,
} from "@mui/material";
import { DApi, ErrorWidget, useApi, useMediaUploadJob } from "../utils";
import {
  ChangeEvent,
  FormEvent,
  useCallback,
  useEffect,
  useState,
} from "react";
import { ApiUserSettings } from "../api";
import { userNameMaxLength } from "../consts";
import { StandardCard } from "../widgets/StandardCard";

import CloudUploadIcon from "@mui/icons-material/CloudUpload";
import { VisuallyHiddenInput } from "../widgets/VisuallyHiddenInput";
import { useAuth } from "../providers/auth";

// FIXME: clean this up
const BasicSettingsWidget = () => {
  const { refetchAuth } = useAuth();
  const [errorBS, fetchSettings, fsIsPending] = useApi(
    DApi.apiUsersSettingsBasicGet,
  );
  const [settings, setSettings] = useState<ApiUserSettings | null>(null);

  useEffect(() => {
    // NOTE: overrides local changes
    fetchSettings(undefined, {
      onSuccess: (s) => {
        setSettings(s.data);
        refetchAuth();
      },
    });
  }, [fetchSettings, refetchAuth]);

  const [errorSS, submitSettings, ssIsPending] = useApi(
    DApi.apiUsersSettingsBasicPut,
  );
  const submit = async (e: FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    if (settings) {
      submitSettings(
        { apiUserSettings: settings },
        { onSuccess: () => fetchSettings() },
      );
      // FIXME: inform about needing to logout and in
    }
  };

  const handleChangeState = (e: React.ChangeEvent<HTMLInputElement>) => {
    // limit length of title to 24 characters
    if (e.target.name === "name" && e.target.value.length > userNameMaxLength) {
      return;
    }
    if (settings) {
      setSettings({
        ...settings,
        [e.target.name]: e.target.value,
      });
    }
  };

  const isPending = ssIsPending || fsIsPending;

  return (
    <StandardCard>
      <CardHeader
        title={
          <Typography gutterBottom variant="h5" align="center">
            Basic settings
          </Typography>
        }
      />
      <CardContent>
        {settings ? (
          <form onSubmit={submit}>
            <Stack spacing={2} alignItems="center">
              <TextField
                required
                name="name"
                label="Username"
                variant="standard"
                value={settings.name}
                onChange={handleChangeState}
                disabled={fsIsPending}
              />
              <Button variant="contained" type="submit" disabled={isPending}>
                Apply
              </Button>
            </Stack>
          </form>
        ) : (
          <Typography>
            Loading... Wer hat die Einstellungen geklaut? O.o
          </Typography>
        )}
        <ErrorWidget errors={[errorBS, errorSS]} />
      </CardContent>
    </StandardCard>
  );
};

// FIXME: clean this up
const ChangePasswordWidget = () => {
  const [state, setState] = useState({
    oldPassword: "",
    newPassword: "",
    newPasswordRepeat: "",
  });
  const [error, setError] = useState<string | null>(null);

  const [errorCP, changePassword, cpIsPending] = useApi(
    DApi.apiUsersSettingsPasswordPut,
  );

  const handleChangeState = (e: React.ChangeEvent<HTMLInputElement>) => {
    // limit length of title to 24 characters
    if (e.target.name === "name" && e.target.value.length > userNameMaxLength) {
      return;
    }
    setState({
      ...state,
      [e.target.name]: e.target.value,
    });
  };

  const submit = async (e: FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    if (state.newPassword !== state.newPasswordRepeat) {
      setError("Passwords do not match");
      return;
    } else {
      setError("");
    }
    if (state.newPassword && state.oldPassword) {
      changePassword({ changePassword: state });
    }
  };

  const isPending = cpIsPending;

  return (
    <StandardCard>
      <CardHeader
        title={
          <Typography gutterBottom variant="h5" align="center">
            Change password
          </Typography>
        }
      />
      <CardContent>
        <form onSubmit={submit}>
          <Stack spacing={2} alignItems="center">
            <TextField
              type="password"
              required
              name="oldPassword"
              label="Old password"
              variant="standard"
              value={state.oldPassword}
              onChange={handleChangeState}
              disabled={isPending}
            />
            <TextField
              type="password"
              required
              name="newPassword"
              label="New password"
              variant="standard"
              value={state.newPassword}
              onChange={handleChangeState}
              disabled={isPending}
            />
            <TextField
              type="password"
              required
              name="newPasswordRepeat"
              label="Repeat new password"
              variant="standard"
              value={state.newPasswordRepeat}
              onChange={handleChangeState}
              disabled={isPending}
            />
            <Button variant="contained" type="submit" disabled={isPending}>
              Apply
            </Button>
          </Stack>
        </form>
        <ErrorWidget errors={[errorCP, error]} />
      </CardContent>
    </StandardCard>
  );
};

// FIXME: clean this up
const ChangeAvatarWidget = () => {
  const { refetchAuth } = useAuth();
  const [error, setError] = useState<string | null>(null);
  const [info, setInfo] = useState<string>("");
  const [isPending, setIsPending] = useState<boolean>(false);

  const [errorUA, uploadAvatar] = useApi(DApi.apiUsersSettingsAvatarPost);

  const onJobFinished = useCallback(() => {
    setInfo("Changed avatar successfully");
    setIsPending(false);
    refetchAuth();
  }, [refetchAuth, setInfo]);

  const onJobFailed = useCallback(() => {
    setIsPending(false);
  }, []);

  const setupMediaUpPolling = useMediaUploadJob({
    setInfo,
    setError,
    onJobFinished,
    onJobFailed,
  });

  const handleFileChange = async (e: ChangeEvent<HTMLInputElement>) => {
    const files = e.target.files;
    if (!files) return;
    setInfo("Uploading media");
    setIsPending(true);
    uploadAvatar(
      { uploadAvatar: { file: files[0] } },
      { onSuccess: (response) => setupMediaUpPolling(response.data) },
    );
  };

  return (
    <StandardCard>
      <CardHeader
        title={
          <Typography gutterBottom variant="h5" align="center">
            Change profile picture
          </Typography>
        }
      />
      <Stack alignItems="center">
        <Button
          component="label"
          variant="contained"
          tabIndex={-1}
          startIcon={<CloudUploadIcon />}
          disabled={isPending}
        >
          Upload media
          <VisuallyHiddenInput type="file" onChange={handleFileChange} />
        </Button>
        <Typography>{info}</Typography>
        <ErrorWidget errors={[error, errorUA]} />
      </Stack>
    </StandardCard>
  );
};

export const SettingsRoute = () => {
  return (
    <Stack spacing={2} alignItems="center">
      <BasicSettingsWidget />
      <ChangePasswordWidget />
      <ChangeAvatarWidget />
    </Stack>
  );
};
