import { IconButton, IconButtonProps, Menu, MenuItem } from "@mui/material";
import ShareIcon from "@mui/icons-material/Share";
import PopupState, { bindTrigger, bindMenu } from "material-ui-popup-state";
import LinkIcon from "@mui/icons-material/Link";

export const ShareMenu = ({ url, title }: { url: string; title: string }) => {
  // Function to handle sharing via the Web Share API
  const handleNativeShare = async () => {
    if (navigator.share) {
      try {
        await navigator.share({
          title: title || "",
          text: "",
          url,
        });
      } catch (error) {
        // FIXME: more information
        // FIXME: inform user
        console.error("Error sharing:", error);
      }
    }
  };

  // Function to handle copying the link
  const handleCopyLink = () => {
    navigator.clipboard.writeText(url);
  };

  // Check if the Web Share API is supported
  const isWebShareSupported = !!navigator.share;
  const iconButton = (props: IconButtonProps) => {
    return (
      <IconButton aria-label="share" {...props}>
        <ShareIcon />
      </IconButton>
    );
  };

  if (isWebShareSupported) {
    return iconButton({
      onClick: () => {
        handleNativeShare();
      },
    });
  } else {
    return (
      <PopupState variant="popover" popupId="share-menu-popup">
        {(popupState) => (
          <>
            {iconButton({
              ...bindTrigger(popupState),
            })}

            <Menu {...bindMenu(popupState)}>
              <MenuItem
                onClick={() => {
                  handleCopyLink();
                  popupState.close();
                }}
              >
                <LinkIcon />
                Copy Link
              </MenuItem>
            </Menu>
          </>
        )}
      </PopupState>
    );
  }
};
