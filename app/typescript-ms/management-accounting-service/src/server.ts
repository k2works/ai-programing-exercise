// src/server.ts

import Fastify from 'fastify'
import cors from '@fastify/cors'
import { config } from './config'
import { getEventSubscriber, closeEventSubscriber } from './infrastructure/messaging/EventSubscriber'
import { PrismaJournalCacheRepository } from './infrastructure/persistence/PrismaJournalCacheRepository'
import { JournalCreatedHandler } from './application/handlers/journal-created-handler'
import { closePrismaClient } from './infrastructure/persistence/prisma-client'

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
        service: 'management-accounting',
        timestamp: new Date().toISOString()
      }
    })

    // ルートエンドポイント
    fastify.get('/', async () => {
      return {
        service: 'Management Accounting Service',
        version: '1.0.0',
        endpoints: {
          health: '/health',
          'financial-analysis': '/financial-analysis/:fiscalYear',
          'compare': '/financial-analysis/compare'
        }
      }
    })

    // EventSubscriber の初期化
    const eventSubscriber = getEventSubscriber()
    await eventSubscriber.connect()

    // イベントハンドラーの登録
    const journalCacheRepository = new PrismaJournalCacheRepository()
    const journalCreatedHandler = new JournalCreatedHandler(journalCacheRepository)

    eventSubscriber.on('journal.created', (event) => journalCreatedHandler.handle(event))

    // イベント購読を開始
    await eventSubscriber.subscribe()

    // サーバー起動
    await fastify.listen({
      port: config.port,
      host: config.host
    })

    console.log(`✅ Management Accounting Service listening on ${config.host}:${config.port}`)
  } catch (err) {
    fastify.log.error(err)
    process.exit(1)
  }
}

// グレースフルシャットダウン
process.on('SIGINT', async () => {
  console.log('🛑 Shutting down gracefully...')
  await fastify.close()
  await closeEventSubscriber()
  await closePrismaClient()
  process.exit(0)
})

process.on('SIGTERM', async () => {
  console.log('🛑 Shutting down gracefully...')
  await fastify.close()
  await closeEventSubscriber()
  await closePrismaClient()
  process.exit(0)
})

// サーバー起動
start()
