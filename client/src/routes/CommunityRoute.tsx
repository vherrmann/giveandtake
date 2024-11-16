import { FriendsWidget } from "../widgets/FriendsWidget";
import { FriendsRequestWidget } from "../widgets/FriendsRequestWidget";
import { Stack } from "@mui/material";
import { GroupsWidget } from "../widgets/GroupsWidget";
import { useTitle } from "../utils";

export default function CommunityRoute() {
  useTitle("Community");
  // FIXME: Add widget for all group join requests
  return (
    <Stack spacing={2}>
      <FriendsWidget />
      <FriendsRequestWidget />
      {/* <GroupJoinRequestWidgets userId={userId} /> */}
      <GroupsWidget />
    </Stack>
  );
}
