'use client';

import {
  Notification,
  NotificationType,
  useNotifications,
} from '@/stores/notifications';

export const Notifications = () => {
  const { notifications, dismissNotification } =
    useNotifications();

  if (notifications.length < 1) return null;

  return (
    <div className="fixed top-12 right-0 z-10 p-4">
      <div className="flex flex-col-reverse gap-4">
        {notifications.map((notification) => (
          <NotificationToast
            key={notification.id}
            notification={notification}
            onDismiss={dismissNotification}
          />
        ))}
      </div>
    </div>
  );
};

const notificationVariants: Record<
  NotificationType,
  { bgColor: string; textColor: string }
> = {
  info: {
    bgColor: 'bg-blue-500',
    textColor: 'text-white',
  },
  success: {
    bgColor: 'bg-green-500',
    textColor: 'text-white',
  },
  warning: {
    bgColor: 'bg-orange-500',
    textColor: 'text-white',
  },
  error: {
    bgColor: 'bg-red-500',
    textColor: 'text-white',
  },
};

type NotificationToastProps = {
  notification: Omit<Notification, 'duration'>;
  onDismiss: (id: string) => void;
};

const NotificationToast = ({
  notification,
  onDismiss,
}: NotificationToastProps) => {
  const { id, type, title, message } = notification;
  const variant = notificationVariants[type];

  return (
    <div
      className={`w-full sm:w-96 shadow-md rounded-lg ${variant.bgColor} ${variant.textColor}`}
    >
      <div className="flex justify-between items-start p-4 space-x-3">
        <div className="flex-1 space-y-2.5">
          <div className="space-y-1">
            <div className="text-sm font-medium">
              {title}
            </div>
            {notification.message && (
              <div className="text-sm opacity-90">
                {message}
              </div>
            )}
          </div>
        </div>
        <button
          onClick={() => onDismiss(id)}
          className="text-current hover:opacity-75 transform -translate-y-1.5"
          aria-label="Close notification"
        >
          <svg
            className="w-4 h-4"
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M6 18L18 6M6 6l12 12"
            />
          </svg>
        </button>
      </div>
    </div>
  );
};