import {
  Button,
  CardContent,
  CardHeader,
  Stack,
  Typography,
} from "@mui/material";
import { useSearchParams } from "react-router-dom";
import { DApi, ErrorWidget, useApi } from "../utils";
import { FormContainer, TextFieldElement } from "react-hook-form-mui";
import { StandardCard } from "../widgets/StandardCard";

export const RequestEmailVerifRoute = () => {
  const [searchParams] = useSearchParams();
  const emailInitial = searchParams.get("email");

  const [errorVR, requestEmailVerif, loadingVR] = useApi(
    DApi.apiAuthVerifyemailRequestPost,
  );

  const onSubmit = async ({ email }: { email: string }) => {
    requestEmailVerif({
      emailVerificationRequest: { email },
    });
  };

  return (
    <StandardCard>
      <CardHeader
        title={
          <Typography variant="h5" align="center">
            Request email verification
          </Typography>
        }
        subheader={
          <Typography gutterBottom variant="subtitle1" align="center">
            (2 additional times allowed + 1 per day)
          </Typography>
        }
      />
      <CardContent>
        <FormContainer
          defaultValues={{ email: emailInitial || "" }}
          onSuccess={onSubmit}
        >
          <Stack spacing={2} alignItems="center">
            <TextFieldElement name="email" label="Email" required />
            <Button variant="contained" type="submit" disabled={loadingVR}>
              Submit
            </Button>
          </Stack>
        </FormContainer>
        <ErrorWidget errors={[errorVR]} />
      </CardContent>
    </StandardCard>
  );
};
