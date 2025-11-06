// src/domain/product.ts
import { prisma } from '../lib/prisma'
import { Product } from '@prisma/client'

export interface CreateProductData {
  prodCode: string
  fullname: string
  name: string
  kana: string
  unitprice: number
  primeCost: number
  supCode: string
}

export interface UpdateProductData {
  fullname?: string
  name?: string
  kana?: string
  unitprice?: number
  primeCost?: number
  supCode?: string
}

export class ProductDomain {
  /**
   * 商品を作成
   */
  async create(data: CreateProductData): Promise<Product> {
    return await prisma.product.create({
      data: {
        ...data,
        taxType: 1 // デフォルト値
      }
    })
  }

  /**
   * すべての商品を取得
   */
  async findAll(): Promise<Product[]> {
    return await prisma.product.findMany()
  }

  /**
   * ID で商品を取得
   */
  async findById(prodCode: string): Promise<Product | null> {
    return await prisma.product.findUnique({
      where: { prodCode }
    })
  }

  /**
   * 商品を更新
   */
  async update(prodCode: string, data: UpdateProductData): Promise<Product> {
    return await prisma.product.update({
      where: { prodCode },
      data
    })
  }

  /**
   * 商品を削除
   */
  async delete(prodCode: string): Promise<void> {
    await prisma.product.delete({
      where: { prodCode }
    })
  }
}
