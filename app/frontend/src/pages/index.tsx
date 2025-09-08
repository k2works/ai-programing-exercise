import type { NextPage } from 'next'
import Head from 'next/head'
import {
  Box,
  Container,
  Heading,
  Text,
  Button,
  SimpleGrid,
  Stack,
  Badge,
  Flex,
  Icon
} from '@chakra-ui/react'
import { FiMapPin, FiClock, FiDollarSign } from 'react-icons/fi'

const Home: NextPage = () => {
  const sampleJobs = [
    {
      id: '1',
      title: 'フロントエンドエンジニア',
      company: '株式会社テックイノベーション',
      location: '東京都渋谷区',
      employmentType: 'フルタイム',
      salaryRange: '600万円〜900万円',
      postedAt: '2日前'
    },
    {
      id: '2',
      title: 'バックエンドエンジニア',
      company: '株式会社クラウドソリューション',
      location: '大阪府大阪市',
      employmentType: 'フルタイム',
      salaryRange: '700万円〜1000万円',
      postedAt: '3日前'
    },
    {
      id: '3',
      title: 'UI/UXデザイナー',
      company: '株式会社デザインスタジオ',
      location: '神奈川県横浜市',
      employmentType: 'リモート',
      salaryRange: '500万円〜800万円',
      postedAt: '1週間前'
    }
  ]

  return (
    <>
      <Head>
        <title>Job Board - 求人検索プラットフォーム</title>
        <meta name="description" content="React Job Board Application - 求人情報管理プラットフォーム" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <Box>
        {/* Hero Section */}
        <Box bg="blue.50" py={20}>
          <Container maxW="container.xl">
            <Stack textAlign="center" spacing={8}>
              <Heading size="2xl" color="gray.800">
                理想の仕事を見つけよう
              </Heading>
              <Text fontSize="xl" color="gray.600" maxW="2xl" mx="auto">
                求人情報管理プラットフォーム。求人検索から応募まで、
                効率的な採用活動をサポートします。
              </Text>
              <Button colorScheme="blue" size="lg" px={8}>
                求人を探す
              </Button>
            </Stack>
          </Container>
        </Box>

        {/* Featured Jobs Section */}
        <Container maxW="container.xl" py={16}>
          <Stack spacing={8}>
            <Heading size="lg" textAlign="center">
              注目の求人
            </Heading>
            
            <SimpleGrid columns={{ base: 1, md: 2, lg: 3 }} spacing={6}>
              {sampleJobs.map((job) => (
                <Box 
                  key={job.id} 
                  p={6} 
                  borderWidth={1} 
                  borderRadius="lg" 
                  _hover={{ shadow: 'md' }} 
                  cursor="pointer"
                >
                  <Stack spacing={4}>
                    <Stack spacing={2}>
                      <Heading size="md">{job.title}</Heading>
                      <Text color="gray.600">{job.company}</Text>
                    </Stack>
                    
                    <Stack spacing={2}>
                      <Flex align="center" gap={2}>
                        <Icon as={FiMapPin} color="gray.500" />
                        <Text fontSize="sm" color="gray.600">{job.location}</Text>
                      </Flex>
                      
                      <Flex align="center" gap={2}>
                        <Icon as={FiDollarSign} color="gray.500" />
                        <Text fontSize="sm" color="gray.600">{job.salaryRange}</Text>
                      </Flex>
                      
                      <Flex align="center" justify="space-between">
                        <Badge colorScheme="blue" variant="subtle">
                          {job.employmentType}
                        </Badge>
                        <Flex align="center" gap={1}>
                          <Icon as={FiClock} color="gray.500" />
                          <Text fontSize="sm" color="gray.500">{job.postedAt}</Text>
                        </Flex>
                      </Flex>
                    </Stack>
                    
                    <Button colorScheme="blue" variant="outline" size="sm">
                      詳細を見る
                    </Button>
                  </Stack>
                </Box>
              ))}
            </SimpleGrid>
            
            <Box textAlign="center">
              <Button variant="outline" size="lg">
                すべての求人を見る
              </Button>
            </Box>
          </Stack>
        </Container>

        {/* Footer */}
        <Box bg="gray.50" py={8}>
          <Container maxW="container.xl">
            <Text textAlign="center" color="gray.600">
              © 2025 Job Board Application. React Application Architecture for Production.
            </Text>
          </Container>
        </Box>
      </Box>
    </>
  )
}

export default Home
