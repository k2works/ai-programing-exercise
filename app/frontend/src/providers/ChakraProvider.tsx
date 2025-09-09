'use client';

import { ChakraProvider as BaseChakraProvider } from '@chakra-ui/react';

interface ChakraProviderProps {
  children: React.ReactNode;
}

export const ChakraProvider: React.FC<ChakraProviderProps> = ({ children }) => {
  return <BaseChakraProvider>{children}</BaseChakraProvider>;
};
