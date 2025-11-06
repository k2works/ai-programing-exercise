// src/application.ts
import Fastify, { FastifyInstance, FastifyRequest, FastifyReply } from 'fastify'
import cors from '@fastify/cors'
import swagger from '@fastify/swagger'
import swaggerUi from '@fastify/swagger-ui'
import { CreateProductSchema, UpdateProductSchema } from './schemas/product'
import { ProductService } from './service/product'
import { ZodSchema } from 'zod'

/**
 * Zod バリデーションヘルパー
 */
function validateRequest<T>(
  schema: ZodSchema<T>,
  body: unknown
): { success: true; data: T } | { success: false; error: unknown } {
  const result = schema.safeParse(body)
  if (result.success) {
    return { success: true, data: result.data }
  } else {
    return { success: false, error: result.error.errors }
  }
}

/**
 * Fastify アプリケーションを構築
 */
export async function buildApp(): Promise<FastifyInstance> {
  const app = Fastify({
    logger: true
  })

  // CORS の設定
  await app.register(cors, {
    origin: true
  })

  // Swagger の設定
  await app.register(swagger, {
    openapi: {
      info: {
        title: 'Sales Management API',
        description: '販売管理システム API ドキュメント',
        version: '1.0.0'
      },
      servers: [
        {
          url: 'http://localhost:3000',
          description: 'Development server'
        }
      ],
      tags: [
        { name: 'products', description: '商品関連 API' },
        { name: 'customers', description: '得意先関連 API' },
        { name: 'suppliers', description: '仕入先関連 API' },
        { name: 'stocks', description: '在庫関連 API' }
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

  // サービス層のインスタンス化（シングルトン）
  const productService = new ProductService()

  /**
   * ルートエンドポイント
   */
  app.get('/', async () => {
    return {
      message: 'Sales Management API',
      version: '1.0.0',
      endpoints: ['/products', '/customers', '/suppliers', '/stocks'],
      docs: '/docs'
    }
  })

  /**
   * ヘルスチェックエンドポイント
   */
  app.get('/health', async () => {
    return { status: 'ok' }
  })

  /**
   * 商品作成エンドポイント
   */
  app.post(
    '/products',
    {
      schema: {
        tags: ['products'],
        summary: '商品を作成',
        body: {
          type: 'object',
          required: ['prodCode', 'fullname', 'name', 'kana', 'unitprice', 'primeCost', 'supCode'],
          properties: {
            prodCode: { type: 'string', maxLength: 16 },
            fullname: { type: 'string', maxLength: 40 },
            name: { type: 'string', maxLength: 10 },
            kana: { type: 'string', maxLength: 20 },
            unitprice: { type: 'integer', minimum: 0 },
            primeCost: { type: 'integer', minimum: 0 },
            supCode: { type: 'string', maxLength: 8 }
          }
        },
        response: {
          201: {
            description: 'Successful response',
            type: 'object',
            properties: {
              prodCode: { type: 'string' },
              fullname: { type: 'string' },
              name: { type: 'string' },
              kana: { type: 'string' },
              unitprice: { type: 'integer' },
              primeCost: { type: 'integer' },
              supCode: { type: 'string' }
            }
          }
        }
      }
    },
    async (request: FastifyRequest, reply: FastifyReply) => {
      try {
        const validation = validateRequest(CreateProductSchema, request.body)
        if (!validation.success) {
          return reply.code(400).send({
            error: 'Validation failed',
            details: validation.error
          })
        }

        const product = await productService.createProduct(validation.data)
        return reply.code(201).send(product)
      } catch (error) {
        return reply.code(500).send({
          error: 'Product creation failed',
          message: error instanceof Error ? error.message : 'Unknown error'
        })
      }
    }
  )

  /**
   * 商品一覧取得エンドポイント
   */
  app.get(
    '/products',
    {
      schema: {
        tags: ['products'],
        summary: 'すべての商品を取得',
        response: {
          200: {
            description: 'Successful response',
            type: 'array',
            items: {
              type: 'object',
              properties: {
                prodCode: { type: 'string' },
                fullname: { type: 'string' },
                name: { type: 'string' },
                kana: { type: 'string' },
                unitprice: { type: 'integer' },
                primeCost: { type: 'integer' },
                supCode: { type: 'string' }
              }
            }
          }
        }
      }
    },
    async (request: FastifyRequest, reply: FastifyReply) => {
      try {
        const products = await productService.getAllProducts()
        return reply.code(200).send(products)
      } catch (error) {
        return reply.code(500).send({
          error: 'Failed to fetch products',
          message: error instanceof Error ? error.message : 'Unknown error'
        })
      }
    }
  )

  /**
   * 商品取得エンドポイント
   */
  app.get(
    '/products/:prodCode',
    {
      schema: {
        tags: ['products'],
        summary: 'ID で商品を取得',
        params: {
          type: 'object',
          required: ['prodCode'],
          properties: {
            prodCode: { type: 'string' }
          }
        },
        response: {
          200: {
            description: 'Successful response',
            type: 'object',
            properties: {
              prodCode: { type: 'string' },
              fullname: { type: 'string' },
              name: { type: 'string' },
              kana: { type: 'string' },
              unitprice: { type: 'integer' },
              primeCost: { type: 'integer' },
              supCode: { type: 'string' }
            }
          },
          404: {
            description: 'Product not found',
            type: 'object',
            properties: {
              error: { type: 'string' }
            }
          }
        }
      }
    },
    async (request: FastifyRequest<{ Params: { prodCode: string } }>, reply: FastifyReply) => {
      try {
        const product = await productService.getProductById(request.params.prodCode)
        if (!product) {
          return reply.code(404).send({ error: 'Product not found' })
        }
        return reply.code(200).send(product)
      } catch (error) {
        return reply.code(500).send({
          error: 'Failed to fetch product',
          message: error instanceof Error ? error.message : 'Unknown error'
        })
      }
    }
  )

  /**
   * 商品更新エンドポイント
   */
  app.put(
    '/products/:prodCode',
    {
      schema: {
        tags: ['products'],
        summary: '商品を更新',
        params: {
          type: 'object',
          required: ['prodCode'],
          properties: {
            prodCode: { type: 'string' }
          }
        },
        body: {
          type: 'object',
          properties: {
            fullname: { type: 'string', maxLength: 40 },
            name: { type: 'string', maxLength: 10 },
            kana: { type: 'string', maxLength: 20 },
            unitprice: { type: 'integer', minimum: 0 },
            primeCost: { type: 'integer', minimum: 0 },
            supCode: { type: 'string', maxLength: 8 }
          }
        },
        response: {
          200: {
            description: 'Successful response',
            type: 'object',
            properties: {
              prodCode: { type: 'string' },
              fullname: { type: 'string' },
              name: { type: 'string' },
              kana: { type: 'string' },
              unitprice: { type: 'integer' },
              primeCost: { type: 'integer' },
              supCode: { type: 'string' }
            }
          }
        }
      }
    },
    async (request: FastifyRequest<{ Params: { prodCode: string } }>, reply: FastifyReply) => {
      try {
        const validation = validateRequest(UpdateProductSchema, request.body)
        if (!validation.success) {
          return reply.code(400).send({
            error: 'Validation failed',
            details: validation.error
          })
        }

        const product = await productService.updateProduct(request.params.prodCode, validation.data)
        return reply.code(200).send(product)
      } catch (error) {
        return reply.code(500).send({
          error: 'Product update failed',
          message: error instanceof Error ? error.message : 'Unknown error'
        })
      }
    }
  )

  /**
   * 商品削除エンドポイント
   */
  app.delete(
    '/products/:prodCode',
    {
      schema: {
        tags: ['products'],
        summary: '商品を削除',
        params: {
          type: 'object',
          required: ['prodCode'],
          properties: {
            prodCode: { type: 'string' }
          }
        },
        response: {
          204: {
            description: 'Successful response',
            type: 'null'
          }
        }
      }
    },
    async (request: FastifyRequest<{ Params: { prodCode: string } }>, reply: FastifyReply) => {
      try {
        await productService.deleteProduct(request.params.prodCode)
        return reply.code(204).send()
      } catch (error) {
        return reply.code(500).send({
          error: 'Product deletion failed',
          message: error instanceof Error ? error.message : 'Unknown error'
        })
      }
    }
  )

  return app
}

/**
 * サーバー起動（CLI から実行する場合）
 */
export async function startServer(): Promise<void> {
  const app = await buildApp()

  try {
    await app.listen({ port: 3000, host: '0.0.0.0' })
    app.log.info('Server is running on http://localhost:3000')
    app.log.info('Swagger UI is available at http://localhost:3000/docs')
  } catch (err) {
    app.log.error(err)
    throw err
  }
}
