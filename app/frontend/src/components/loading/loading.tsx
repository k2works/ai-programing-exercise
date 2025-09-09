import { Spinner, Center, Text, VStack } from '@chakra-ui/react';

export type LoadingProps = {
  text?: string;
  size?: 'sm' | 'md' | 'lg' | 'xl';
};

export const Loading = ({ text = 'Loading...', size = 'md' }: LoadingProps) => {
  return (
    <Center h="200px">
      <VStack spacing={4}>
        <Spinner size={size} color="primary" />
        <Text color="gray.600">{text}</Text>
      </VStack>
    </Center>
  );
};
