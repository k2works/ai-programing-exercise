// src/server.ts

import Fastify from 'fastify'
import cors from '@fastify/cors'
import { config } from './config'
import { PrismaAccountRepository } from './infrastructure/persistence/PrismaAccountRepository'
import { PrismaJournalRepository } from './infrastructure/persistence/PrismaJournalRepository'
import { accountController } from './presentation/controllers/account-controller'
import { journalController } from './presentation/controllers/journal-controller'
import { closePrismaClient } from './infrastructure/persistence/prisma-client'
import { getEventPublisher, closeEventPublisher } from './infrastructure/messaging/EventPublisher'

const fastify = Fastify({
  logger: {
    level: config.isDevelopment ? 'info' : 'warn'
  }
})

async function start(): Promise<void> {
  try {
    // CORS設定
    await fastify.register(cors, {
      origin: config.corsOrigin
    })

    // ヘルスチェックエンドポイント
    fastify.get('/health', async () => {
      return {
        status: 'ok',
        service: 'financial-accounting',
        timestamp: new Date().toISOString()
      }
    })

    // ルートエンドポイント
    fastify.get('/', async () => {
      return {
        service: 'Financial Accounting Service',
        version: '1.0.0',
        endpoints: {
          health: '/health',
          accounts: '/accounts',
          journals: '/journals',
          'financial-statements': '/financial-statements',
          'audit-logs': '/audit-logs'
        }
      }
    })

    // EventPublisher の初期化
    const eventPublisher = getEventPublisher()
    await eventPublisher.connect()

    // リポジトリのインスタンス化
    const accountRepository = new PrismaAccountRepository()
    const journalRepository = new PrismaJournalRepository()

    // コントローラーの登録
    await accountController(fastify, accountRepository)
    await journalController(fastify, journalRepository, eventPublisher)

    // サーバー起動
    await fastify.listen({
      port: config.port,
      host: config.host
    })

    console.log(`✅ Financial Accounting Service listening on ${config.host}:${config.port}`)
  } catch (err) {
    fastify.log.error(err)
    process.exit(1)
  }
}

// グレースフルシャットダウン
process.on('SIGINT', async () => {
  console.log('🛑 Shutting down gracefully...')
  await fastify.close()
  await closePrismaClient()
  await closeEventPublisher()
  process.exit(0)
})

process.on('SIGTERM', async () => {
  console.log('🛑 Shutting down gracefully...')
  await fastify.close()
  await closePrismaClient()
  await closeEventPublisher()
  process.exit(0)
})

// サーバー起動
start()
