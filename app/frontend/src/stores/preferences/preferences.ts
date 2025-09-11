import { create } from 'zustand';
import { persist } from '../middleware';

export type UserPreferencesStore = {
  // Language and locale
  language: 'en' | 'ja';
  timezone: string;
  
  // Display preferences
  dateFormat: 'ISO' | 'US' | 'EU' | 'JP';
  timeFormat: '12h' | '24h';
  
  // Notification preferences
  emailNotifications: boolean;
  pushNotifications: boolean;
  soundEnabled: boolean;
  
  // UI preferences
  compactMode: boolean;
  animationsEnabled: boolean;
  
  // Actions
  setLanguage: (language: 'en' | 'ja') => void;
  setTimezone: (timezone: string) => void;
  setDateFormat: (format: 'ISO' | 'US' | 'EU' | 'JP') => void;
  setTimeFormat: (format: '12h' | '24h') => void;
  setEmailNotifications: (enabled: boolean) => void;
  setPushNotifications: (enabled: boolean) => void;
  setSoundEnabled: (enabled: boolean) => void;
  setCompactMode: (enabled: boolean) => void;
  setAnimationsEnabled: (enabled: boolean) => void;
  resetToDefaults: () => void;
};

const defaultPreferences = {
  language: 'en' as const,
  timezone: 'UTC',
  dateFormat: 'ISO' as const,
  timeFormat: '24h' as const,
  emailNotifications: true,
  pushNotifications: true,
  soundEnabled: true,
  compactMode: false,
  animationsEnabled: true,
};

export const userPreferencesStore = create<UserPreferencesStore>(
  persist<UserPreferencesStore>(
    (set) => ({
      ...defaultPreferences,
      
      setLanguage: (language) => set({ language }),
      setTimezone: (timezone) => set({ timezone }),
      setDateFormat: (dateFormat) => set({ dateFormat }),
      setTimeFormat: (timeFormat) => set({ timeFormat }),
      setEmailNotifications: (emailNotifications) => set({ emailNotifications }),
      setPushNotifications: (pushNotifications) => set({ pushNotifications }),
      setSoundEnabled: (soundEnabled) => set({ soundEnabled }),
      setCompactMode: (compactMode) => set({ compactMode }),
      setAnimationsEnabled: (animationsEnabled) => set({ animationsEnabled }),
      
      resetToDefaults: () => set(defaultPreferences),
    }),
    { 
      name: 'user-preferences',
      storage: 'localStorage'
    }
  )
);