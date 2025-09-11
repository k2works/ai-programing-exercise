import { useStore } from 'zustand';

import { uiStore } from './ui';

// Basic selectors
export const useSidebarState = () => 
  useStore(uiStore, (state) => state.sidebarOpen);

export const useThemeMode = () => 
  useStore(uiStore, (state) => state.colorMode);

export const useMobileMenuState = () => 
  useStore(uiStore, (state) => state.mobileMenuOpen);

// Layout state selector with shallow comparison
export const useLayoutState = () =>
  useStore(uiStore, (state) => ({
    sidebarOpen: state.sidebarOpen,
    mobileMenuOpen: state.mobileMenuOpen,
  })) as {
    sidebarOpen: boolean;
    mobileMenuOpen: boolean;
  };

// Modal selectors
export const useModalState = (modalId: string) =>
  useStore(uiStore, (state) => state.modals[modalId] || { isOpen: false }) as {
    isOpen: boolean;
    data?: unknown;
  };

export const useHasOpenModals = () =>
  useStore(uiStore, (state) => 
    Object.values(state.modals).some(modal => modal.isOpen)
  );

// UI actions hooks
export const useUIActions = () =>
  useStore(uiStore, (state) => ({
    toggleSidebar: state.toggleSidebar,
    setMobileMenu: state.setMobileMenu,
    setColorMode: state.setColorMode,
    openModal: state.openModal,
    closeModal: state.closeModal,
  })) as {
    toggleSidebar: () => void;
    setMobileMenu: (open: boolean) => void;
    setColorMode: (mode: 'light' | 'dark' | 'system') => void;
    openModal: (id: string, data?: unknown) => void;
    closeModal: (id: string) => void;
  };