import { ChakraProvider } from '@chakra-ui/react';
import { render, RenderOptions } from '@testing-library/react';
import { ReactElement } from 'react';

import { theme } from '@/config/theme';

// Chakra UI付きでコンポーネントをレンダリングするヘルパー
const AllTheProviders = ({ children }: { children: React.ReactNode }) => {
  return <ChakraProvider theme={theme}>{children}</ChakraProvider>;
};

const customRender = (
  ui: ReactElement,
  options?: Omit<RenderOptions, 'wrapper'>
) => render(ui, { wrapper: AllTheProviders, ...options });

export * from '@testing-library/react';
export { customRender as renderWithChakra };
