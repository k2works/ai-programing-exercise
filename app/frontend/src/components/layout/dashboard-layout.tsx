import {
  Box,
  Container,
} from '@chakra-ui/react';
import { ReactNode } from 'react';

import { Link } from '../link';
import { Navbar } from '../navbar';
import { useUser } from '../../testing/test-data';

type DashboardLayoutProps = {
  children: ReactNode;
};

export const DashboardLayout = ({
  children,
}: DashboardLayoutProps) => {
  const user = useUser();

  return (
    <Box as="section" h="100vh" overflowY="auto">
      <Navbar />
      <Container as="main" maxW="container.lg" py="12">
        {children}
      </Container>
      <Box py="8" textAlign="center">
        <Link
          href={`/organizations/${user.data?.organizationId}`}
        >
          View Public Organization Page
        </Link>
      </Box>
    </Box>
  );
};