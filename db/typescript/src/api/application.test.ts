// src/api/application.test.ts
import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { FastifyInstance } from 'fastify'
import { buildApp } from './application'
import { prisma } from './lib/prisma'

describe('Application Layer', () => {
  let app: FastifyInstance

  beforeAll(async () => {
    app = await buildApp()
  })

  afterAll(async () => {
    await app.close()
    await prisma.$disconnect()
  })

  beforeEach(async () => {
    // テストデータのクリーンアップ
    await prisma.product.deleteMany()
  })

  describe('GET /', () => {
    it('ルートエンドポイントが API 情報を返す', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/'
      })

      expect(response.statusCode).toBe(200)
      const data = JSON.parse(response.body)
      expect(data.message).toBe('Sales Management API')
      expect(data.version).toBe('1.0.0')
    })
  })

  describe('GET /health', () => {
    it('ヘルスチェックエンドポイントが正常ステータスを返す', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/health'
      })

      expect(response.statusCode).toBe(200)
      expect(JSON.parse(response.body)).toEqual({ status: 'ok' })
    })
  })

  describe('POST /products', () => {
    it('商品を作成できる', async () => {
      const response = await app.inject({
        method: 'POST',
        url: '/products',
        payload: {
          prodCode: 'TEST00001',
          fullname: 'テスト商品',
          name: 'テスト',
          kana: 'テストショウヒン',
          unitprice: 1000,
          primeCost: 700,
          supCode: 'SUP00001'
        }
      })

      expect(response.statusCode).toBe(201)
      const data = JSON.parse(response.body)
      expect(data.prodCode).toBe('TEST00001')
    })

    it('バリデーションエラーを返す', async () => {
      const response = await app.inject({
        method: 'POST',
        url: '/products',
        payload: {
          prodCode: 'TEST00001',
          // fullname が欠落
          name: 'テスト',
          kana: 'テストショウヒン',
          unitprice: 1000,
          primeCost: 700,
          supCode: 'SUP00001'
        }
      })

      expect(response.statusCode).toBe(400)
    })
  })

  describe('GET /products', () => {
    it('すべての商品を取得できる', async () => {
      // テストデータを投入
      await app.inject({
        method: 'POST',
        url: '/products',
        payload: {
          prodCode: 'TEST00001',
          fullname: 'テスト商品1',
          name: 'テスト1',
          kana: 'テストショウヒン1',
          unitprice: 1000,
          primeCost: 700,
          supCode: 'SUP00001'
        }
      })

      const response = await app.inject({
        method: 'GET',
        url: '/products'
      })

      expect(response.statusCode).toBe(200)
      const data = JSON.parse(response.body)
      expect(data).toHaveLength(1)
    })
  })

  describe('GET /products/:prodCode', () => {
    it('ID で商品を取得できる', async () => {
      await app.inject({
        method: 'POST',
        url: '/products',
        payload: {
          prodCode: 'TEST00001',
          fullname: 'テスト商品',
          name: 'テスト',
          kana: 'テストショウヒン',
          unitprice: 1000,
          primeCost: 700,
          supCode: 'SUP00001'
        }
      })

      const response = await app.inject({
        method: 'GET',
        url: '/products/TEST00001'
      })

      expect(response.statusCode).toBe(200)
      const data = JSON.parse(response.body)
      expect(data.prodCode).toBe('TEST00001')
    })

    it('存在しない ID の場合 404 を返す', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/products/NONEXISTENT'
      })

      expect(response.statusCode).toBe(404)
    })
  })

  describe('PUT /products/:prodCode', () => {
    it('商品を更新できる', async () => {
      await app.inject({
        method: 'POST',
        url: '/products',
        payload: {
          prodCode: 'TEST00001',
          fullname: 'テスト商品',
          name: 'テスト',
          kana: 'テストショウヒン',
          unitprice: 1000,
          primeCost: 700,
          supCode: 'SUP00001'
        }
      })

      const response = await app.inject({
        method: 'PUT',
        url: '/products/TEST00001',
        payload: {
          unitprice: 1500
        }
      })

      expect(response.statusCode).toBe(200)
      const data = JSON.parse(response.body)
      expect(data.unitprice).toBe(1500)
    })
  })

  describe('DELETE /products/:prodCode', () => {
    it('商品を削除できる', async () => {
      await app.inject({
        method: 'POST',
        url: '/products',
        payload: {
          prodCode: 'TEST00001',
          fullname: 'テスト商品',
          name: 'テスト',
          kana: 'テストショウヒン',
          unitprice: 1000,
          primeCost: 700,
          supCode: 'SUP00001'
        }
      })

      const response = await app.inject({
        method: 'DELETE',
        url: '/products/TEST00001'
      })

      expect(response.statusCode).toBe(204)

      // 削除確認
      const getResponse = await app.inject({
        method: 'GET',
        url: '/products/TEST00001'
      })
      expect(getResponse.statusCode).toBe(404)
    })
  })
})
