'use client';

import { DashboardLayout } from '@/layouts';
import { Button, Link } from '@/components';
import { 
  useUIActions, 
  useSidebarState, 
  useLayoutState,
  useModalState,
  useThemeMode 
} from '@/stores/ui';
import { 
  useNotificationActions, 
  useNotificationCount, 
  useHasNotifications 
} from '@/stores/notifications';
import { 
  usePreferencesActions, 
  useLanguage, 
  useTimezone, 
  useDateFormat, 
  useTimeFormat,
  useNotificationPreferences,
  useUIPreferences,
  useLocalePreferences 
} from '@/stores/preferences';

function UIStateDemo() {
  const sidebarOpen = useSidebarState();
  const themeMode = useThemeMode();
  const layoutState = useLayoutState();
  const modalState = useModalState('demo-modal');
  const { 
    toggleSidebar, 
    setMobileMenu, 
    setColorMode, 
    openModal, 
    closeModal 
  } = useUIActions();

  return (
    <section className="space-y-6">
      <h2 className="text-2xl font-bold text-gray-900 text-center">
        UI State Management
      </h2>
      
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <div className="bg-white p-6 rounded-lg shadow">
          <h3 className="text-lg font-semibold mb-4">Layout Controls</h3>
          
          <div className="space-y-4">
            <div className="flex justify-between items-center">
              <span className="text-sm font-medium">Sidebar:</span>
              <span className="text-sm text-gray-800">{sidebarOpen ? 'open' : 'closed'}</span>
            </div>
            
            <div className="flex justify-between items-center">
              <span className="text-sm font-medium">Mobile Menu:</span>
              <span className="text-sm text-gray-800">{layoutState.mobileMenuOpen ? 'true' : 'false'}</span>
            </div>
            
            <div className="space-y-2">
              <Button 
                onClick={toggleSidebar}
                variant="outline"
                size="sm"
                className="w-full"
              >
                Toggle Sidebar
              </Button>
              
              <Button 
                onClick={() => setMobileMenu(!layoutState.mobileMenuOpen)}
                variant="outline"
                size="sm"
                className="w-full"
              >
                Toggle Mobile Menu
              </Button>
            </div>
          </div>
        </div>

        <div className="bg-white p-6 rounded-lg shadow">
          <h3 className="text-lg font-semibold mb-4">Theme & Modals</h3>
          
          <div className="space-y-4">
            <div className="flex justify-between items-center">
              <span className="text-sm font-medium">Color Mode:</span>
              <span className="text-sm text-gray-800">{themeMode}</span>
            </div>
            
            <div className="flex justify-between items-center">
              <span className="text-sm font-medium">Modal Open:</span>
              <span className="text-sm text-gray-800">{modalState.isOpen ? 'true' : 'false'}</span>
            </div>
            
            <div className="space-y-2">
              {(['light', 'dark', 'system'] as const).map((mode) => (
                <Button
                  key={mode}
                  onClick={() => setColorMode(mode)}
                  variant={themeMode === mode ? 'solid' : 'outline'}
                  size="sm"
                  className="w-full"
                >
                  Set {mode.charAt(0).toUpperCase() + mode.slice(1)} Mode
                </Button>
              ))}
              
              <Button 
                onClick={() => openModal('demo-modal', { 
                  title: 'Demo Modal', 
                  content: 'This is a test modal with data!' 
                })}
                variant="solid"
                size="sm"
                className="w-full"
              >
                Open Modal
              </Button>
              
              <Button 
                onClick={() => closeModal('demo-modal')}
                variant="outline"
                size="sm"
                className="w-full"
                isDisabled={!modalState.isOpen}
              >
                Close Modal
              </Button>
            </div>
            
            {modalState.isOpen && modalState.data ? (
              <div className="bg-gray-50 p-4 rounded border">
                <h4 className="font-medium">{(modalState.data as { title: string; content: string }).title}</h4>
                <p className="text-sm text-gray-800">{(modalState.data as { title: string; content: string }).content}</p>
              </div>
            ) : null}
          </div>
        </div>
      </div>
    </section>
  );
}

function NotificationsDemo() {
  const notificationCount = useNotificationCount();
  const hasNotifications = useHasNotifications();
  const { showNotification } = useNotificationActions();

  const showTestNotification = (type: 'info' | 'success' | 'warning' | 'error') => {
    showNotification({
      type,
      message: `This is a test ${type} notification.`,
      duration: type === 'error' ? undefined : 5000,
    });
  };

  return (
    <section className="space-y-6">
      <h2 className="text-2xl font-bold text-gray-900 text-center">
        Notifications
      </h2>
      
      <div className="bg-white p-6 rounded-lg shadow">
        <h3 className="text-lg font-semibold mb-4">Notification System</h3>
        
        <div className="space-y-4">
          <div className="flex justify-between items-center">
            <span className="text-sm font-medium">Active Notifications:</span>
            <span className="text-sm text-gray-800">{notificationCount}</span>
          </div>
          
          <div className="flex justify-between items-center">
            <span className="text-sm font-medium">Has Notifications:</span>
            <span className="text-sm text-gray-800">{hasNotifications ? 'Yes' : 'No'}</span>
          </div>
          
          <div className="grid grid-cols-2 gap-2">
            {(['info', 'success', 'warning', 'error'] as const).map((type) => (
              <Button
                key={type}
                onClick={() => showTestNotification(type)}
                variant="outline"
                size="sm"
                className={`capitalize ${
                  type === 'error' ? 'bg-red-50 text-red-600 hover:bg-red-100' :
                  type === 'warning' ? 'bg-yellow-50 text-yellow-600 hover:bg-yellow-100' :
                  type === 'success' ? 'bg-green-50 text-green-600 hover:bg-green-100' :
                  'bg-blue-50 text-blue-600 hover:bg-blue-100'
                }`}
              >
                {type} Notification
              </Button>
            ))}
          </div>
        </div>
      </div>
    </section>
  );
}

function UserPreferencesDemo() {
  const language = useLanguage();
  const timezone = useTimezone();
  const dateFormat = useDateFormat();
  const timeFormat = useTimeFormat();
  const notificationPrefs = useNotificationPreferences();
  const uiPrefs = useUIPreferences();
  const localePrefs = useLocalePreferences();
  
  const { 
    setLanguage, 
    setDateFormat, 
    setTimeFormat,
    setEmailNotifications,
    setPushNotifications,
    setSoundEnabled,
    setCompactMode,
    setAnimationsEnabled,
    resetToDefaults 
  } = usePreferencesActions();

  // Use store values directly instead of local state to avoid infinite loop
  // const [selectedLang, setSelectedLang] = useState<string>(language);
  // const [selectedDateFormat, setSelectedDateFormat] = useState<string>(dateFormat);
  // const [selectedTimeFormat, setSelectedTimeFormat] = useState<string>(timeFormat);

  return (
    <section className="space-y-6">
      <h2 className="text-2xl font-bold text-gray-900 text-center">
        User Preferences (with localStorage persistence)
      </h2>
      
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        <div className="bg-white p-6 rounded-lg shadow">
          <h3 className="text-lg font-semibold mb-4">Locale Settings</h3>
          
          <div className="space-y-4">
            <div>
              <label className="block text-sm font-medium mb-1">Language</label>
              <select 
                value={language}
                onChange={(e) => {
                  setLanguage(e.target.value);
                }}
                className="w-full border rounded px-3 py-2 text-sm"
              >
                <option value="en">English</option>
                <option value="ja">Japanese</option>
                <option value="es">Spanish</option>
                <option value="fr">French</option>
              </select>
            </div>
            
            <div>
              <label className="block text-sm font-medium mb-1">Timezone</label>
              <div className="text-sm text-gray-800 bg-gray-50 p-2 rounded">
                {timezone}
              </div>
            </div>
            
            <div>
              <label className="block text-sm font-medium mb-1">Date Format</label>
              <select 
                value={dateFormat}
                onChange={(e) => {
                  setDateFormat(e.target.value);
                }}
                className="w-full border rounded px-3 py-2 text-sm"
              >
                <option value="ISO">ISO (YYYY-MM-DD)</option>
                <option value="US">US (MM/DD/YYYY)</option>
                <option value="EU">EU (DD/MM/YYYY)</option>
              </select>
            </div>
            
            <div>
              <label className="block text-sm font-medium mb-1">Time Format</label>
              <select 
                value={timeFormat}
                onChange={(e) => {
                  setTimeFormat(e.target.value);
                }}
                className="w-full border rounded px-3 py-2 text-sm"
              >
                <option value="24h">24 Hour</option>
                <option value="12h">12 Hour (AM/PM)</option>
              </select>
            </div>
          </div>
        </div>

        <div className="bg-white p-6 rounded-lg shadow">
          <h3 className="text-lg font-semibold mb-4">Notification Preferences</h3>
          
          <div className="space-y-4">
            <label className="flex items-center space-x-3">
              <input 
                type="checkbox"
                checked={notificationPrefs.emailNotifications}
                onChange={(e) => setEmailNotifications(e.target.checked)}
                className="rounded"
              />
              <span className="text-sm">Email Notifications</span>
            </label>
            
            <label className="flex items-center space-x-3">
              <input 
                type="checkbox"
                checked={notificationPrefs.pushNotifications}
                onChange={(e) => setPushNotifications(e.target.checked)}
                className="rounded"
              />
              <span className="text-sm">Push Notifications</span>
            </label>
            
            <label className="flex items-center space-x-3">
              <input 
                type="checkbox"
                checked={notificationPrefs.soundEnabled}
                onChange={(e) => setSoundEnabled(e.target.checked)}
                className="rounded"
              />
              <span className="text-sm">Sound Effects</span>
            </label>
          </div>
        </div>

        <div className="bg-white p-6 rounded-lg shadow">
          <h3 className="text-lg font-semibold mb-4">UI Preferences</h3>
          
          <div className="space-y-4">
            <label className="flex items-center space-x-3">
              <input 
                type="checkbox"
                checked={uiPrefs.compactMode}
                onChange={(e) => setCompactMode(e.target.checked)}
                className="rounded"
              />
              <span className="text-sm">Compact Mode</span>
            </label>
            
            <label className="flex items-center space-x-3">
              <input 
                type="checkbox"
                checked={uiPrefs.animationsEnabled}
                onChange={(e) => setAnimationsEnabled(e.target.checked)}
                className="rounded"
              />
              <span className="text-sm">Animations</span>
            </label>
            
            <Button 
              onClick={resetToDefaults}
              variant="outline"
              size="sm"
              className="w-full mt-4"
            >
              Reset to Defaults
            </Button>
          </div>
        </div>
      </div>
      
      <div className="bg-gray-50 p-4 rounded-lg">
        <h4 className="font-medium mb-2">Current Preferences Summary</h4>
        <div className="text-sm text-gray-800 space-y-1">
          <p><strong>Locale:</strong> {localePrefs.language} | {localePrefs.timezone} | {localePrefs.dateFormat} | {localePrefs.timeFormat}</p>
          <p><strong>Notifications:</strong> Email: {notificationPrefs.emailNotifications ? 'On' : 'Off'} | Push: {notificationPrefs.pushNotifications ? 'On' : 'Off'} | Sound: {notificationPrefs.soundEnabled ? 'On' : 'Off'}</p>
          <p><strong>UI:</strong> Compact: {uiPrefs.compactMode ? 'On' : 'Off'} | Animations: {uiPrefs.animationsEnabled ? 'On' : 'Off'}</p>
        </div>
      </div>
    </section>
  );
}

export default function ClientStateDemo() {
  return (
    <DashboardLayout>
      <div className="space-y-12">
        <div className="text-center">
          <h1 className="text-4xl font-bold text-gray-900 mb-4">
            Client State Management Demo
          </h1>
          <p className="text-lg text-gray-800 max-w-3xl mx-auto">
            Comprehensive demonstration of Zustand-based client state management including 
            UI state, notifications, and persisted user preferences with localStorage integration.
          </p>
          <div className="mt-6">
            <Link href="/demo" className="text-blue-600 hover:text-blue-800">
              ← Back to Demo Overview
            </Link>
          </div>
        </div>

        <UIStateDemo />
        <NotificationsDemo />
        <UserPreferencesDemo />

        <div className="text-center pt-8 border-t">
          <p className="text-sm text-gray-500">
            Open browser DevTools and check the Redux tab to see state changes in real-time!
          </p>
        </div>
      </div>
    </DashboardLayout>
  );
}