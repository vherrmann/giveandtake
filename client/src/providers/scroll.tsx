import Box from "@mui/material/Box";
import { ReactNode, RefObject, createContext, useContext, useRef } from "react";
export type ScrollContextType = {
  scrollRef: RefObject<ReactNode>;
};

const ScrollContext = createContext<ScrollContextType | undefined>(undefined);

export const ScrollProvider = ({ children }: { children: ReactNode }) => {
  const scrollRef = useRef<ReactNode>(null);

  return (
    <Box sx={{ flex: "1 1", overflow: "auto" }} ref={scrollRef}>
      <ScrollContext.Provider value={{ scrollRef }}>
        {children}
      </ScrollContext.Provider>
    </Box>
  );
};

export const useScroll = () => {
  const context = useContext(ScrollContext);
  if (!context) {
    throw new Error("useScroll must be used within an ScrollProvider");
  }
  return context;
};
