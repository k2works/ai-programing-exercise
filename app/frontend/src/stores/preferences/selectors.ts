import { useStore } from 'zustand';

import { userPreferencesStore } from './preferences';

// Basic preference selectors
export const useLanguage = () =>
  useStore(userPreferencesStore, (state) => state?.language || 'en');

export const useTimezone = () =>
  useStore(userPreferencesStore, (state) => state?.timezone || 'UTC');

export const useDateFormat = () =>
  useStore(userPreferencesStore, (state) => state?.dateFormat || 'ISO');

export const useTimeFormat = () =>
  useStore(userPreferencesStore, (state) => state?.timeFormat || '24h');

// Notification preferences
export const useNotificationPreferences = () =>
  useStore(userPreferencesStore, (state) => ({
    emailNotifications: state?.emailNotifications ?? true,
    pushNotifications: state?.pushNotifications ?? true,
    soundEnabled: state?.soundEnabled ?? true,
  })) as {
    emailNotifications: boolean;
    pushNotifications: boolean;
    soundEnabled: boolean;
  };

// UI preferences
export const useUIPreferences = () =>
  useStore(userPreferencesStore, (state) => ({
    compactMode: state?.compactMode ?? false,
    animationsEnabled: state?.animationsEnabled ?? true,
  })) as {
    compactMode: boolean;
    animationsEnabled: boolean;
  };

// Locale preferences (combined)
export const useLocalePreferences = () =>
  useStore(userPreferencesStore, (state) => ({
    language: state?.language || 'en',
    timezone: state?.timezone || 'UTC',
    dateFormat: state?.dateFormat || 'ISO',
    timeFormat: state?.timeFormat || '24h',
  })) as {
    language: string;
    timezone: string;
    dateFormat: string;
    timeFormat: string;
  };

// Actions hook
export const usePreferencesActions = () =>
  useStore(userPreferencesStore, (state) => ({
    setLanguage: state?.setLanguage || (() => {}),
    setTimezone: state?.setTimezone || (() => {}),
    setDateFormat: state?.setDateFormat || (() => {}),
    setTimeFormat: state?.setTimeFormat || (() => {}),
    setEmailNotifications: state?.setEmailNotifications || (() => {}),
    setPushNotifications: state?.setPushNotifications || (() => {}),
    setSoundEnabled: state?.setSoundEnabled || (() => {}),
    setCompactMode: state?.setCompactMode || (() => {}),
    setAnimationsEnabled: state?.setAnimationsEnabled || (() => {}),
    resetToDefaults: state?.resetToDefaults || (() => {}),
  })) as {
    setLanguage: (language: string) => void;
    setTimezone: (timezone: string) => void;
    setDateFormat: (format: string) => void;
    setTimeFormat: (format: string) => void;
    setEmailNotifications: (enabled: boolean) => void;
    setPushNotifications: (enabled: boolean) => void;
    setSoundEnabled: (enabled: boolean) => void;
    setCompactMode: (enabled: boolean) => void;
    setAnimationsEnabled: (enabled: boolean) => void;
    resetToDefaults: () => void;
  };