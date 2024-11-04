import { FriendsWidget } from "../widgets/FriendsWidget";
import { useAuthedState } from "../ProtectedRoute";
import { FriendsRequestWidget } from "../widgets/FriendsRequestWidget";
import { Stack } from "@mui/material";

export default function CommunityRoute() {
  const { userId } = useAuthedState();

  return (
    <Stack spacing={2}>
      {" "}
      <FriendsWidget userId={userId} />
      <FriendsRequestWidget userId={userId} />
    </Stack>
  );
}
