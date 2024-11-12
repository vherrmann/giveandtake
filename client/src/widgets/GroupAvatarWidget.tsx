import { GroupPublic } from "../api";
import { DApi, useApiState } from "../utils";
import { AvatarBase, AvatarBaseParams } from "./AvatarBase";

type GroupAvatarBaseParams = { groupId: string } & Omit<
  AvatarBaseParams,
  "toBase" | "id"
>;

const GroupAvatarBase = ({ groupId, ...props }: GroupAvatarBaseParams) => (
  <AvatarBase id={groupId} toBase={"/group/"} {...props} />
);

export const GroupAvatarWidget = ({
  groupId,
  groupPublic: mGroupPublic,
  ...props
}: {
  groupId: string;
  // if groupPublic is undefined, AvatarWidget fetches it
  // if groupPublic is null, the parent will(/or can) fetch it
  groupPublic?: GroupPublic | null;
} & GroupAvatarBaseParams) => {
  const [_errorUG, fetchedGroupPublic] = useApiState(
    DApi.apiGroupsIdPublicGet,
    mGroupPublic === undefined ? { id: groupId } : null,
  );
  const groupPublic = mGroupPublic || fetchedGroupPublic;

  return (
    <GroupAvatarBase groupId={groupId} {...props}>
      {groupPublic?.name[0] || "?"}
    </GroupAvatarBase>
  );
};
