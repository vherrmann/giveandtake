import { UserPublic } from "../api";
import { DApi, useApiState } from "../utils";
import { Img } from "./Img";
import { AvatarBase, AvatarBaseParams } from "./AvatarBase";

type UserAvatarBaseParams = { userId: string } & Omit<
  AvatarBaseParams,
  "toBase" | "id"
>;

const UserAvatarBase = ({ userId, ...props }: UserAvatarBaseParams) => (
  <AvatarBase id={userId} toBase={"/user/"} {...props} />
);

export const UserAvatarWidget = ({
  userId,
  userPublic: mUserPublic,
  ...props
}: {
  userId: string;
  // if userPublic is undefined, AvatarWidget fetches it
  // if userPublic is null, the parent will(/or can) fetch it
  userPublic?: UserPublic;
} & UserAvatarBaseParams) => {
  const [_errorUG, fetchedUserPublic] = useApiState(
    DApi.apiUsersIdGet,
    mUserPublic === undefined ? { id: userId } : null,
  );
  const userPublic = mUserPublic || fetchedUserPublic;

  const [_errorA, avatar] = useApiState(
    DApi.apiMediaIdGet,
    userPublic && userPublic.avatar ? { id: userPublic.avatar } : null,
  );

  return (
    <UserAvatarBase userId={userId} {...props}>
      {avatar && userPublic ? (
        <Img
          media={avatar}
          alt={"profile picture of " + userPublic.name}
          cover
        />
      ) : (
        userPublic?.name[0] || "?"
      )}
    </UserAvatarBase>
  );
};
