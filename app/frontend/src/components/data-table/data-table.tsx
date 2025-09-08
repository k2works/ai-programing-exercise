import { ReactNode } from 'react';
import {
  Table,
  Thead,
  Tbody,
  Tr,
  Th,
  Td,
  Box,
  Spinner,
  Text,
  Center,
} from '@chakra-ui/react';

export type DataTableColumn<T> = {
  title: string;
  field: keyof T;
  render?: ({ entry }: { entry: T }) => ReactNode;
};

export type DataTableProps<T> = {
  data: T[];
  columns: DataTableColumn<T>[];
  isLoading?: boolean;
};

export const DataTable = <T extends Record<string, any>>({
  data,
  columns,
  isLoading = false,
}: DataTableProps<T>) => {
  if (isLoading) {
    return (
      <Center h="200px">
        <Box textAlign="center">
          <Spinner size="lg" color="primary" mb={4} />
          <Text color="gray.600">Loading...</Text>
        </Box>
      </Center>
    );
  }

  if (!data.length) {
    return (
      <Center h="200px">
        <Text color="gray.600" fontSize="lg">
          No data available
        </Text>
      </Center>
    );
  }

  return (
    <Box overflowX="auto">
      <Table variant="simple">
        <Thead bg="gray.50">
          <Tr>
            {columns.map((column, index) => (
              <Th key={index} color="gray.700" fontWeight="semibold">
                {column.title}
              </Th>
            ))}
          </Tr>
        </Thead>
        <Tbody>
          {data.map((entry, rowIndex) => (
            <Tr key={rowIndex} _hover={{ bg: 'gray.50' }}>
              {columns.map((column, colIndex) => (
                <Td key={colIndex} py={4}>
                  {column.render 
                    ? column.render({ entry }) 
                    : String(entry[column.field] || '')
                  }
                </Td>
              ))}
            </Tr>
          ))}
        </Tbody>
      </Table>
    </Box>
  );
};