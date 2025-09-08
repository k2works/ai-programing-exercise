import {
  ChakraProvider,
  VStack,
  Heading,
  Text,
  Button,
} from '@chakra-ui/react';
import { QueryClientProvider } from '@tanstack/react-query';
import { ReactQueryDevtools } from '@tanstack/react-query-devtools';
import { ReactNode } from 'react';
import { ErrorBoundary, ErrorBoundaryPropsWithFallback } from 'react-error-boundary';

import { theme } from '@/config/theme';
import { queryClient } from '@/lib/react-query';

type AppProviderProps = {
  children: ReactNode;
};

const ErrorFallback = ({ error, resetErrorBoundary }: any) => {
  return (
    <VStack spacing={4} p={8}>
      <Heading as="h2" size="lg" color="red.500">
        Something went wrong
      </Heading>
      <Text color="gray.600">
        {error.message}
      </Text>
      <Button onClick={resetErrorBoundary} colorScheme="blue">
        Try again
      </Button>
    </VStack>
  );
};

export const AppProvider = ({
  children,
}: AppProviderProps) => {
  return (
    <ChakraProvider theme={theme}>
      <ErrorBoundary
        FallbackComponent={ErrorFallback}
        onError={(error, errorInfo) => {
          console.error('Application Error:', error, errorInfo);
        }}
        onReset={() => {
          window.location.reload();
        }}
      >
        <QueryClientProvider client={queryClient}>
          <ReactQueryDevtools initialIsOpen={false} />
          {children}
        </QueryClientProvider>
      </ErrorBoundary>
    </ChakraProvider>
  );
};
