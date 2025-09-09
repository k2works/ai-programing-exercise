'use client';

import { ChakraProvider } from './ChakraProvider';
import { QueryProvider } from './QueryProvider';

interface AppProvidersProps {
  children: React.ReactNode;
}

export const AppProviders: React.FC<AppProvidersProps> = ({ children }) => {
  return (
    <QueryProvider>
      <ChakraProvider>{children}</ChakraProvider>
    </QueryProvider>
  );
};
