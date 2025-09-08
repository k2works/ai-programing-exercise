import { 
  Alert, 
  AlertIcon, 
  AlertTitle, 
  AlertDescription,
  CloseButton,
  VStack,
  Box
} from '@chakra-ui/react';

import { useNotifications } from '@/stores/notifications';

export const NotificationCenter = () => {
  const { notifications, dismissNotification } = useNotifications();

  if (notifications.length === 0) {
    return null;
  }

  return (
    <Box position="fixed" top={4} right={4} zIndex="toast">
      <VStack spacing={2} align="flex-end">
        {notifications.map((notification) => (
          <Alert
            key={notification.id}
            status={notification.type}
            variant="solid"
            borderRadius="md"
            boxShadow="lg"
            maxWidth="400px"
          >
            <AlertIcon />
            <Box flex="1">
              <AlertTitle>{notification.title}</AlertTitle>
              {notification.message && (
                <AlertDescription>
                  {notification.message}
                </AlertDescription>
              )}
            </Box>
            <CloseButton
              position="absolute"
              right="8px"
              top="8px"
              onClick={() => dismissNotification(notification.id)}
            />
          </Alert>
        ))}
      </VStack>
    </Box>
  );
};
