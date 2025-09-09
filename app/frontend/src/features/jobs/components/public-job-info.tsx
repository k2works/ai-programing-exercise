import {
  Box,
  Heading,
  Text,
  VStack,
  HStack,
  Badge,
  Divider,
  Stack,
} from '@chakra-ui/react';

import { Job } from '../types';

export type PublicJobInfoProps = {
  job: Job;
};

export const PublicJobInfo = ({ job }: PublicJobInfoProps) => {
  return (
    <Box maxW="4xl" mx="auto" p={6}>
      <VStack spacing={6} align="stretch">
        {/* Header */}
        <Box>
          <Heading as="h1" size="xl" color="primary" mb={2}>
            {job.position}
          </Heading>
          <HStack spacing={4} flexWrap="wrap">
            <Badge colorScheme="blue" variant="subtle" px={3} py={1}>
              {job.department}
            </Badge>
            <Badge colorScheme="green" variant="subtle" px={3} py={1}>
              {job.location}
            </Badge>
          </HStack>
        </Box>

        <Divider />

        {/* Job Information */}
        <Box>
          <Heading as="h2" size="md" mb={4}>
            Job Description
          </Heading>
          <Text color="gray.700" lineHeight="tall" whiteSpace="pre-wrap">
            {job.info}
          </Text>
        </Box>

        <Divider />

        {/* Additional Details */}
        <Stack direction={['column', 'row']} spacing={6}>
          <Box flex={1}>
            <Text fontSize="sm" color="gray.600" mb={1}>
              Department
            </Text>
            <Text fontWeight="medium">{job.department}</Text>
          </Box>
          <Box flex={1}>
            <Text fontSize="sm" color="gray.600" mb={1}>
              Location
            </Text>
            <Text fontWeight="medium">{job.location}</Text>
          </Box>
          <Box flex={1}>
            <Text fontSize="sm" color="gray.600" mb={1}>
              Posted
            </Text>
            <Text fontWeight="medium">
              {new Date(job.createdAt).toLocaleDateString()}
            </Text>
          </Box>
        </Stack>
      </VStack>
    </Box>
  );
};
