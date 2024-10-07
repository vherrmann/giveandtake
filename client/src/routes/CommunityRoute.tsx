import { FriendsWidgets } from "../widgets/FriendsWidget";
import { useAuthedState } from "../ProtectedRoute";
import { FriendsRequestWidgets } from "../widgets/FriendsRequestWidgets";
import { Stack } from "@mui/material";

export default function CommunityRoute() {
  const { userId } = useAuthedState();

  return (
    <Stack spacing={2}>
      {" "}
      <FriendsWidgets userId={userId} />
      <FriendsRequestWidgets userId={userId} />
    </Stack>
  );
}
