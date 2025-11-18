// src/server.ts

import Fastify from 'fastify'
import cors from '@fastify/cors'
import { config } from './config'

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
  process.exit(0)
})

process.on('SIGTERM', async () => {
  console.log('🛑 Shutting down gracefully...')
  await fastify.close()
  process.exit(0)
})

// サーバー起動
start()
