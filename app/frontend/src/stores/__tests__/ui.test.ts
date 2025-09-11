/**
 * @jest-environment jsdom
 */
import { act, renderHook } from '@testing-library/react';
import { 
  uiStore, 
  useSidebarState, 
  useUIActions, 
  useLayoutState,
  useModalState 
} from '../ui';

describe('UI Store', () => {
  beforeEach(() => {
    // Reset store before each test
    uiStore.setState({
      sidebarOpen: true,
      mobileMenuOpen: false,
      colorMode: 'system',
      modals: {},
    });
  });

  describe('Sidebar functionality', () => {
    it('should toggle sidebar state', () => {
      const { result } = renderHook(() => useSidebarState());
      const { result: actionsResult } = renderHook(() => useUIActions());

      expect(result.current).toBe(true);

      act(() => {
        actionsResult.current.toggleSidebar();
      });

      expect(result.current).toBe(false);

      act(() => {
        actionsResult.current.toggleSidebar();
      });

      expect(result.current).toBe(true);
    });
  });

  describe('Mobile menu functionality', () => {
    it('should set mobile menu state', () => {
      const { result: actionsResult } = renderHook(() => useUIActions());

      act(() => {
        actionsResult.current.setMobileMenu(true);
      });

      const state = uiStore.getState();
      expect(state.mobileMenuOpen).toBe(true);

      act(() => {
        actionsResult.current.setMobileMenu(false);
      });

      expect(uiStore.getState().mobileMenuOpen).toBe(false);
    });
  });

  describe('Color mode functionality', () => {
    it('should set color mode', () => {
      const { result: actionsResult } = renderHook(() => useUIActions());

      act(() => {
        actionsResult.current.setColorMode('dark');
      });

      expect(uiStore.getState().colorMode).toBe('dark');

      act(() => {
        actionsResult.current.setColorMode('light');
      });

      expect(uiStore.getState().colorMode).toBe('light');
    });
  });

  describe('Modal functionality', () => {
    it('should open and close modals', () => {
      const { result: modalResult } = renderHook(() => useModalState('test-modal'));
      const { result: actionsResult } = renderHook(() => useUIActions());

      expect(modalResult.current.isOpen).toBe(false);

      act(() => {
        actionsResult.current.openModal('test-modal', { title: 'Test Modal' });
      });

      expect(modalResult.current.isOpen).toBe(true);
      expect(modalResult.current.data).toEqual({ title: 'Test Modal' });

      act(() => {
        actionsResult.current.closeModal('test-modal');
      });

      expect(modalResult.current.isOpen).toBe(false);
      expect(modalResult.current.data).toBeUndefined();
    });

    it('should handle multiple modals independently', () => {
      const { result: modal1Result } = renderHook(() => useModalState('modal-1'));
      const { result: modal2Result } = renderHook(() => useModalState('modal-2'));
      const { result: actionsResult } = renderHook(() => useUIActions());

      act(() => {
        actionsResult.current.openModal('modal-1');
        actionsResult.current.openModal('modal-2');
      });

      expect(modal1Result.current.isOpen).toBe(true);
      expect(modal2Result.current.isOpen).toBe(true);

      act(() => {
        actionsResult.current.closeModal('modal-1');
      });

      expect(modal1Result.current.isOpen).toBe(false);
      expect(modal2Result.current.isOpen).toBe(true);
    });
  });

  describe('Layout state selector', () => {
    it('should return layout state with shallow comparison', () => {
      const { result } = renderHook(() => useLayoutState());
      const { result: actionsResult } = renderHook(() => useUIActions());

      expect(result.current).toEqual({
        sidebarOpen: true,
        mobileMenuOpen: false,
      });

      act(() => {
        actionsResult.current.toggleSidebar();
        actionsResult.current.setMobileMenu(true);
      });

      expect(result.current).toEqual({
        sidebarOpen: false,
        mobileMenuOpen: true,
      });
    });
  });
});