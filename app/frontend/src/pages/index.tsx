import type { NextPage } from 'next'
import {
  Box,
  Container,
  Heading,
  Text,
  Stack,
} from '@chakra-ui/react'

import { Button } from '@/components/button'
import { Link } from '@/components/link'
import { SEO } from '@/components/seo'
import { JobsList, type Job } from '@/features/jobs'

const Home: NextPage = () => {
  const sampleJobs: Job[] = [
    {
      id: '1',
      organizationId: 'org-1',
      position: 'フロントエンドエンジニア',
      department: 'エンジニアリング',
      location: '東京都渋谷区',
      info: 'React、TypeScript を使用した Web アプリケーション開発。モダンな技術スタックでユーザー体験を向上させるフロントエンド開発をお任せします。',
      createdAt: '2025-09-06T00:00:00Z',
      updatedAt: '2025-09-06T00:00:00Z'
    },
    {
      id: '2',
      organizationId: 'org-2',
      position: 'バックエンドエンジニア',
      department: 'エンジニアリング',
      location: '大阪府大阪市',
      info: 'Node.js、Python を使用したサーバーサイド開発。スケーラブルで高パフォーマンスな API 設計・実装を担当していただきます。',
      createdAt: '2025-09-05T00:00:00Z',
      updatedAt: '2025-09-05T00:00:00Z'
    },
    {
      id: '3',
      organizationId: 'org-3',
      position: 'UI/UXデザイナー',
      department: 'デザイン',
      location: '神奈川県横浜市',
      info: 'ユーザー中心設計によるプロダクト体験の向上。Figma を使用したデザインシステム構築とプロトタイプ作成を行います。',
      createdAt: '2025-09-01T00:00:00Z',
      updatedAt: '2025-09-01T00:00:00Z'
    }
  ]

  return (
    <>
      <SEO 
        title="Job Board - 求人検索プラットフォーム"
        description="React Job Board Application - 求人情報管理プラットフォーム。理想の仕事を見つけて、効率的な採用活動をサポートします。"
      />

      <Box as="main" role="main">
        {/* Hero Section */}
        <Box bg="gray.50" py={20}>
          <Container maxW="container.xl">
            <Stack textAlign="center" spacing={8}>
              <Heading size="2xl" color="primary">
                理想の仕事を見つけよう
              </Heading>
              <Text fontSize="xl" color="gray.600" maxW="2xl" mx="auto">
                求人情報管理プラットフォーム。求人検索から応募まで、
                効率的な採用活動をサポートします。
              </Text>
              <Link href="/jobs">
                <Button variant="solid">
                  求人を探す
                </Button>
              </Link>
            </Stack>
          </Container>
        </Box>

        {/* Featured Jobs Section */}
        <Box as="section" aria-labelledby="jobs-heading">
          <Container maxW="container.xl" py={16}>
            <Stack spacing={8}>
              <Heading id="jobs-heading" size="lg" textAlign="center" as="h2">
                注目の求人
              </Heading>
              
              <JobsList 
                type="public"
                jobs={sampleJobs}
                organizationId="org-public"
                isLoading={false}
              />
              
              <Box textAlign="center">
                <Link href="/jobs">
                  <Button variant="outline">
                    すべての求人を見る
                  </Button>
                </Link>
              </Box>
            </Stack>
          </Container>
        </Box>

        {/* Footer */}
        <Box as="footer" role="contentinfo" bg="primary" color="primaryAccent" py={8}>
          <Container maxW="container.xl">
            <Text textAlign="center">
              © 2025 Job Board Application. Powered by{' '}
              <Link variant="solid" href="/">
                Jobs App
              </Link>
            </Text>
          </Container>
        </Box>
      </Box>
    </>
  )
}

export default Home
