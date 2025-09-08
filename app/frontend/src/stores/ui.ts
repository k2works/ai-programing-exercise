import { createStore, useStore } from 'zustand';
import shallow from 'zustand/shallow';

export type UIStore = {
  // Layout
  sidebarOpen: boolean;
  mobileMenuOpen: boolean;
  
  // Theme
  colorMode: 'light' | 'dark' | 'system';
  
  // Modals
  modals: {
    [key: string]: {
      isOpen: boolean;
      data?: any;
    };
  };
  
  // Actions
  toggleSidebar: () => void;
  setMobileMenu: (open: boolean) => void;
  setColorMode: (mode: 'light' | 'dark' | 'system') => void;
  openModal: (id: string, data?: any) => void;
  closeModal: (id: string) => void;
};

export const uiStore = createStore<UIStore>((set, get) => ({
  sidebarOpen: true,
  mobileMenuOpen: false,
  colorMode: 'system',
  modals: {},
  
  toggleSidebar: () => 
    set((state) => ({ sidebarOpen: !state.sidebarOpen })),
    
  setMobileMenu: (open) => 
    set({ mobileMenuOpen: open }),
    
  setColorMode: (mode) => 
    set({ colorMode: mode }),
    
  openModal: (id, data) =>
    set((state) => ({
      modals: {
        ...state.modals,
        [id]: { isOpen: true, data },
      },
    })),
    
  closeModal: (id) =>
    set((state) => ({
      modals: {
        ...state.modals,
        [id]: { isOpen: false, data: undefined },
      },
    })),
}));

// Basic selectors
export const useSidebarState = () => 
  useStore(uiStore, (state) => state.sidebarOpen);

export const useThemeMode = () => 
  useStore(uiStore, (state) => state.colorMode);

// Shallow comparison for multiple values
export const useLayoutState = () =>
  useStore(uiStore, (state) => ({
    sidebarOpen: state.sidebarOpen,
    mobileMenuOpen: state.mobileMenuOpen,
  }), shallow);

export const useUIState = () =>
  useStore(uiStore, (state) => ({
    sidebarOpen: state.sidebarOpen,
    colorMode: state.colorMode,
  }), shallow);

// Modal selectors
export const useModal = (id: string) =>
  useStore(uiStore, (state) => state.modals[id] || { isOpen: false });

export const useUI = () => useStore(uiStore);
