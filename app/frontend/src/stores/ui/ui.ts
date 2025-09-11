import { create } from 'zustand';

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
      data?: unknown;
    };
  };
  
  // Actions
  toggleSidebar: () => void;
  setMobileMenu: (open: boolean) => void;
  setColorMode: (mode: 'light' | 'dark' | 'system') => void;
  openModal: (id: string, data?: unknown) => void;
  closeModal: (id: string) => void;
};

export const uiStore = create<UIStore>((set) => ({
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