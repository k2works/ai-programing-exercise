// src/api/application.ts
import Fastify, { FastifyInstance } from 'fastify'
import cors from '@fastify/cors'
import swagger from '@fastify/swagger'
import swaggerUi from '@fastify/swagger-ui'
import { accountRoutes } from './routes/account.routes'

/**
 * Fastify アプリケーションを構築
 */
export async function buildApp(): Promise<FastifyInstance> {
  const app = Fastify({
    logger: true
  })

  // CORS の設定
  await app.register(cors, {
    origin: true // 開発環境用、本番環境では適切に制限する
  })

  // Swagger の設定
  await app.register(swagger, {
    openapi: {
      info: {
        title: '財務会計システム API',
        description: 'TDDで育てる財務会計システムの API ドキュメント',
        version: '1.0.0'
      },
      servers: [
        {
          url: 'http://localhost:3000',
          description: '開発サーバー'
        }
      ],
      tags: [
        { name: 'accounts', description: '勘定科目関連 API' },
        { name: 'journals', description: '仕訳関連 API' },
        { name: 'balances', description: '残高関連 API' },
        { name: 'statements', description: '財務諸表関連 API' }
      ]
    }
  })

  // Swagger UI の設定
  await app.register(swaggerUi, {
    routePrefix: '/docs',
    uiConfig: {
      docExpansion: 'list',
      deepLinking: true
    },
    staticCSP: true
  })

  /**
   * ルートエンドポイント
   */
  app.get('/', async () => {
    return {
      message: '財務会計システム API',
      version: '1.0.0',
      endpoints: ['/accounts', '/journals', '/balances', '/statements'],
      docs: '/docs'
    }
  })

  /**
   * ヘルスチェックエンドポイント
   */
  app.get('/health', async () => {
    return { status: 'ok', timestamp: new Date().toISOString() }
  })

  // API ルートの登録
  await app.register(accountRoutes)

  return app
}

/**
 * サーバーを起動
 */
export async function startServer(port = 3000): Promise<FastifyInstance> {
  const app = await buildApp()

  try {
    await app.listen({ port, host: '0.0.0.0' })
    app.log.info(`サーバーが http://localhost:${port} で起動しました`)
    app.log.info(`API仕様は http://localhost:${port}/docs で確認できます`)
  } catch (err) {
    app.log.error(err)
    process.exit(1)
  }

  return app
}
