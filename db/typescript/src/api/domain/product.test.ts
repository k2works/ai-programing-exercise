// src/api/domain/product.test.ts
import { describe, it, expect, beforeEach } from 'vitest'
import { ProductDomain } from './product'
import { prisma } from '../lib/prisma'

describe('ProductDomain', () => {
  let domain: ProductDomain

  beforeEach(async () => {
    domain = new ProductDomain()
    // テスト用データのクリーンアップ
    await prisma.product.deleteMany()
  })

  describe('create', () => {
    it('商品を作成できる', async () => {
      const product = await domain.create({
        prodCode: 'TEST00001',
        fullname: 'テスト商品',
        name: 'テスト',
        kana: 'テストショウヒン',
        unitprice: 1000,
        primeCost: 700,
        supCode: 'SUP00001'
      })

      expect(product.prodCode).toBe('TEST00001')
      expect(product.fullname).toBe('テスト商品')
    })
  })

  describe('findAll', () => {
    it('すべての商品を取得できる', async () => {
      await domain.create({
        prodCode: 'TEST00001',
        fullname: 'テスト商品1',
        name: 'テスト1',
        kana: 'テストショウヒン1',
        unitprice: 1000,
        primeCost: 700,
        supCode: 'SUP00001'
      })

      await domain.create({
        prodCode: 'TEST00002',
        fullname: 'テスト商品2',
        name: 'テスト2',
        kana: 'テストショウヒン2',
        unitprice: 2000,
        primeCost: 1400,
        supCode: 'SUP00001'
      })

      const products = await domain.findAll()
      expect(products).toHaveLength(2)
    })
  })

  describe('findById', () => {
    it('ID で商品を取得できる', async () => {
      await domain.create({
        prodCode: 'TEST00001',
        fullname: 'テスト商品',
        name: 'テスト',
        kana: 'テストショウヒン',
        unitprice: 1000,
        primeCost: 700,
        supCode: 'SUP00001'
      })

      const product = await domain.findById('TEST00001')
      expect(product).not.toBeNull()
      expect(product?.prodCode).toBe('TEST00001')
    })

    it('存在しない ID の場合 null を返す', async () => {
      const product = await domain.findById('NONEXISTENT')
      expect(product).toBeNull()
    })
  })

  describe('update', () => {
    it('商品を更新できる', async () => {
      await domain.create({
        prodCode: 'TEST00001',
        fullname: 'テスト商品',
        name: 'テスト',
        kana: 'テストショウヒン',
        unitprice: 1000,
        primeCost: 700,
        supCode: 'SUP00001'
      })

      const updated = await domain.update('TEST00001', {
        unitprice: 1500
      })

      expect(updated.unitprice).toBe(1500)
    })
  })

  describe('delete', () => {
    it('商品を削除できる', async () => {
      await domain.create({
        prodCode: 'TEST00001',
        fullname: 'テスト商品',
        name: 'テスト',
        kana: 'テストショウヒン',
        unitprice: 1000,
        primeCost: 700,
        supCode: 'SUP00001'
      })

      await domain.delete('TEST00001')

      const product = await domain.findById('TEST00001')
      expect(product).toBeNull()
    })
  })
})
