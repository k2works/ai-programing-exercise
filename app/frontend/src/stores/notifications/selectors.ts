import { useStore } from 'zustand';

import { notificationsStore } from './notifications';

// Basic notification selectors
export const useNotificationCount = () =>
  useStore(notificationsStore, (state) => state.notifications.length);

export const useHasNotifications = () =>
  useStore(notificationsStore, (state) => state.notifications.length > 0);

// Filtered notification selectors
export const useErrorNotifications = () =>
  useStore(notificationsStore, (state) => 
    state.notifications.filter(n => n.type === 'error')
  );

export const useSuccessNotifications = () =>
  useStore(notificationsStore, (state) => 
    state.notifications.filter(n => n.type === 'success')
  );

export const useActiveNotifications = () =>
  useStore(notificationsStore, (state) => 
    state.notifications.filter(n => n.type === 'error' || n.type === 'warning')
  );

// Notification actions hook
export const useNotificationActions = () =>
  useStore(notificationsStore, (state) => ({
    showNotification: state.showNotification,
    dismissNotification: state.dismissNotification,
  })) as {
    showNotification: (notification: { type: 'success' | 'error' | 'warning' | 'info'; message: string; autoDismiss?: boolean; duration?: number }) => string;
    dismissNotification: (id: string) => void;
  };