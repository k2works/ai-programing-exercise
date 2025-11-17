// src/api/__tests__/account-api.test.ts
import { describe, it, expect, beforeAll, afterAll, beforeEach } from 'vitest'
import { FastifyInstance } from 'fastify'
import { buildApp } from '../application'
import { TestDatabase } from '../../test-setup/database'

/**
 * 勘定科目 API 統合テスト
 * 実際の HTTP リクエストと TestContainers データベースを使用
 */
describe('Account API 統合テスト', () => {
  let app: FastifyInstance
  let testDb: TestDatabase

  beforeAll(async () => {
    testDb = new TestDatabase()
    await testDb.start()

    // テスト用の Prisma インスタンスを Fastify アプリケーションに注入
    app = await buildApp({ prisma: testDb.prisma! })
    await app.ready()
  })

  afterAll(async () => {
    await app.close()
    await testDb.stop()
  })

  beforeEach(async () => {
    await testDb.cleanup()
  })

  describe('POST /accounts - 勘定科目作成', () => {
    it('正常系: 勘定科目を作成できる', async () => {
      const response = await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '1110',
          accountName: '現金',
          accountKana: 'ゲンキン',
          accountType: '資産',
          bsplDistinction: 'B',
          transactionDistinction: '1',
          displayOrder: 1,
          aggregationTarget: true
        }
      })

      expect(response.statusCode).toBe(201)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)
      expect(body.data.account.accountCode).toBe('1110')
      expect(body.data.account.accountName).toBe('現金')
      expect(body.data.account.accountType).toBe('資産')
    })

    it('異常系: 重複した科目コードはエラー', async () => {
      // 1回目: 成功
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '1110',
          accountName: '現金',
          accountType: '資産',
          bsplDistinction: 'B'
        }
      })

      // 2回目: 重複エラー
      const response = await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '1110',
          accountName: '現金預金',
          accountType: '資産',
          bsplDistinction: 'B'
        }
      })

      expect(response.statusCode).toBe(409)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(false)
      expect(body.code).toBe('DUPLICATE')
      expect(body.error).toContain('既に使用されています')
    })

    it('異常系: BSPL区分と科目区分の不整合', async () => {
      const response = await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '1110',
          accountName: '現金',
          accountType: '資産',
          bsplDistinction: 'P' // 資産科目なのに損益計算書区分
        }
      })

      expect(response.statusCode).toBe(400)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(false)
      expect(body.code).toBe('VALIDATION_ERROR')
      expect(body.error).toContain('BSPL区分')
    })

    it('異常系: 無効な科目区分', async () => {
      const response = await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '1110',
          accountName: '現金',
          accountType: '無効な区分'
        }
      })

      expect(response.statusCode).toBe(400)
    })
  })

  describe('GET /accounts - 全勘定科目取得', () => {
    it('正常系: 登録された全ての勘定科目を取得できる', async () => {
      // テストデータ作成
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: { accountCode: '1110', accountName: '現金', accountType: '資産' }
      })
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: { accountCode: '2110', accountName: '買掛金', accountType: '負債' }
      })
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: { accountCode: '4110', accountName: '売上高', accountType: '収益' }
      })

      // 取得
      const response = await app.inject({
        method: 'GET',
        url: '/accounts'
      })

      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)
      expect(body.data.accounts).toHaveLength(3)
      expect(body.data.accounts.map((a: { accountCode: string }) => a.accountCode)).toEqual(
        expect.arrayContaining(['1110', '2110', '4110'])
      )
    })

    it('正常系: 勘定科目が0件の場合は空配列を返す', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/accounts'
      })

      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)
      expect(body.data.accounts).toHaveLength(0)
    })
  })

  describe('GET /accounts/:code - 科目コード検索', () => {
    it('正常系: 指定した科目コードの勘定科目を取得できる', async () => {
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '1110',
          accountName: '現金',
          accountKana: 'ゲンキン',
          accountType: '資産',
          bsplDistinction: 'B'
        }
      })

      const response = await app.inject({
        method: 'GET',
        url: '/accounts/1110'
      })

      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)
      expect(body.data).toBeDefined()
      expect(body.data.account).toBeDefined()
      expect(body.data.account.accountCode).toBe('1110')
      expect(body.data.account.accountName).toBe('現金')
      expect(body.data.account.accountKana).toBe('ゲンキン')
    })

    it('異常系: 存在しない科目コードは404エラー', async () => {
      const response = await app.inject({
        method: 'GET',
        url: '/accounts/9999'
      })

      expect(response.statusCode).toBe(404)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(false)
      expect(body.code).toBe('NOT_FOUND')
      expect(body.error).toContain('が見つかりません')
    })
  })

  describe('GET /accounts/bspl/:distinction - BSPL区分検索', () => {
    it('正常系: 貸借対照表科目（B）を取得できる', async () => {
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '1110',
          accountName: '現金',
          accountType: '資産',
          bsplDistinction: 'B'
        }
      })
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '2110',
          accountName: '買掛金',
          accountType: '負債',
          bsplDistinction: 'B'
        }
      })
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '4110',
          accountName: '売上高',
          accountType: '収益',
          bsplDistinction: 'P'
        }
      })

      const response = await app.inject({
        method: 'GET',
        url: '/accounts/bspl/B'
      })

      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)
      expect(body.data.accounts).toHaveLength(2)
      expect(body.data.accounts.map((a: { accountCode: string }) => a.accountCode)).toEqual([
        '1110',
        '2110'
      ])
    })

    it('正常系: 損益計算書科目（P）を取得できる', async () => {
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '4110',
          accountName: '売上高',
          accountType: '収益',
          bsplDistinction: 'P'
        }
      })
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '5110',
          accountName: '給与',
          accountType: '費用',
          bsplDistinction: 'P'
        }
      })

      const response = await app.inject({
        method: 'GET',
        url: '/accounts/bspl/P'
      })

      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)
      expect(body.data.accounts).toHaveLength(2)
    })
  })

  describe('PUT /accounts/:code - 勘定科目更新', () => {
    it('正常系: 勘定科目名を更新できる', async () => {
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '1110',
          accountName: '現金',
          accountType: '資産'
        }
      })

      const response = await app.inject({
        method: 'PUT',
        url: '/accounts/1110',
        payload: {
          accountName: '現金預金',
          accountKana: 'ゲンキンヨキン'
        }
      })

      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)
      expect(body.data.account.accountCode).toBe('1110')
      expect(body.data.account.accountName).toBe('現金預金')
      expect(body.data.account.accountKana).toBe('ゲンキンヨキン')
    })

    it('正常系: 表示順を更新できる', async () => {
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '1110',
          accountName: '現金',
          accountType: '資産'
        }
      })

      const response = await app.inject({
        method: 'PUT',
        url: '/accounts/1110',
        payload: {
          displayOrder: 100
        }
      })

      expect(response.statusCode).toBe(200)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(true)
      expect(body.data.account.displayOrder).toBe(100)
    })

    it('異常系: 存在しない科目コードは404エラー', async () => {
      const response = await app.inject({
        method: 'PUT',
        url: '/accounts/9999',
        payload: {
          accountName: '存在しない科目'
        }
      })

      expect(response.statusCode).toBe(404)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(false)
      expect(body.code).toBe('NOT_FOUND')
    })

    it('異常系: 無効な科目名（空文字）はエラー', async () => {
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '1110',
          accountName: '現金',
          accountType: '資産'
        }
      })

      const response = await app.inject({
        method: 'PUT',
        url: '/accounts/1110',
        payload: {
          accountName: ''
        }
      })

      expect(response.statusCode).toBe(400)
    })
  })

  describe('DELETE /accounts/:code - 勘定科目削除', () => {
    it('正常系: 勘定科目を削除できる', async () => {
      await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '1110',
          accountName: '現金',
          accountType: '資産'
        }
      })

      const response = await app.inject({
        method: 'DELETE',
        url: '/accounts/1110'
      })

      expect(response.statusCode).toBe(204)

      // 削除確認
      const getResponse = await app.inject({
        method: 'GET',
        url: '/accounts/1110'
      })
      expect(getResponse.statusCode).toBe(404)
    })

    it('異常系: 存在しない科目コードは404エラー', async () => {
      const response = await app.inject({
        method: 'DELETE',
        url: '/accounts/9999'
      })

      expect(response.statusCode).toBe(404)
      const body = JSON.parse(response.body)
      expect(body.success).toBe(false)
      expect(body.code).toBe('NOT_FOUND')
    })
  })

  describe('ビジネスルール検証', () => {
    it('貸借対照表科目は BSPL区分 "B" が必須', async () => {
      const assetResponse = await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '1110',
          accountName: '現金',
          accountType: '資産',
          bsplDistinction: 'P' // 誤り
        }
      })
      expect(assetResponse.statusCode).toBe(400)

      const liabilityResponse = await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '2110',
          accountName: '買掛金',
          accountType: '負債',
          bsplDistinction: 'P' // 誤り
        }
      })
      expect(liabilityResponse.statusCode).toBe(400)
    })

    it('損益計算書科目は BSPL区分 "P" が必須', async () => {
      const revenueResponse = await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '4110',
          accountName: '売上高',
          accountType: '収益',
          bsplDistinction: 'B' // 誤り
        }
      })
      expect(revenueResponse.statusCode).toBe(400)

      const expenseResponse = await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '5110',
          accountName: '給与',
          accountType: '費用',
          bsplDistinction: 'B' // 誤り
        }
      })
      expect(expenseResponse.statusCode).toBe(400)
    })

    it('取引要素区分は科目区分と整合している必要がある', async () => {
      // 資産科目に「1」以外の取引要素区分 → エラー
      const response = await app.inject({
        method: 'POST',
        url: '/accounts',
        payload: {
          accountCode: '1110',
          accountName: '現金',
          accountType: '資産',
          transactionDistinction: '2' // 負債用の区分
        }
      })
      expect(response.statusCode).toBe(400)
    })
  })
})
