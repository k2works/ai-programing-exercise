import { 
  Box, 
  Text, 
  VStack, 
  Icon,
  Center 
} from '@chakra-ui/react';
import { QuestionOutlineIcon } from '@chakra-ui/icons';

import { Button } from '../button';
import { Link } from '../link';

export type NotFoundProps = {
  title?: string;
  message?: string;
  actionLabel?: string;
  actionHref?: string;
};

export const NotFound = ({
  title = 'Page Not Found',
  message = 'The page you are looking for does not exist.',
  actionLabel = 'Go Back Home',
  actionHref = '/'
}: NotFoundProps) => {
  return (
    <Center h="400px">
      <VStack spacing={6} textAlign="center">
        <Icon as={QuestionOutlineIcon} boxSize={16} color="gray.400" />
        <VStack spacing={2}>
          <Text fontSize="2xl" fontWeight="bold" color="gray.800">
            {title}
          </Text>
          <Text color="gray.600" maxW="md">
            {message}
          </Text>
        </VStack>
        <Link href={actionHref}>
          <Button variant="solid">
            {actionLabel}
          </Button>
        </Link>
      </VStack>
    </Center>
  );
};