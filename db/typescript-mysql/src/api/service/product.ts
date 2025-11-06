// src/service/product.ts
import { ProductDomain, CreateProductData, UpdateProductData } from '../domain/product'
import { Product } from '@prisma/client'

export class ProductService {
  private domain: ProductDomain

  constructor() {
    this.domain = new ProductDomain()
  }

  /**
   * 商品を作成
   */
  async createProduct(data: CreateProductData): Promise<Product> {
    // ビジネスルールの適用（例：単価が原価より安い場合はエラー）
    if (data.unitprice < data.primeCost) {
      throw new Error('販売単価が売上原価より低い設定はできません')
    }

    return await this.domain.create(data)
  }

  /**
   * すべての商品を取得
   */
  async getAllProducts(): Promise<Product[]> {
    return await this.domain.findAll()
  }

  /**
   * ID で商品を取得
   */
  async getProductById(prodCode: string): Promise<Product | null> {
    return await this.domain.findById(prodCode)
  }

  /**
   * 商品を更新
   */
  async updateProduct(prodCode: string, data: UpdateProductData): Promise<Product> {
    const existing = await this.domain.findById(prodCode)
    if (!existing) {
      throw new Error('商品が見つかりません')
    }

    // ビジネスルールの適用
    const newUnitprice = data.unitprice ?? existing.unitprice
    const newPrimeCost = data.primeCost ?? existing.primeCost

    if (newUnitprice < newPrimeCost) {
      throw new Error('販売単価が売上原価より低い設定はできません')
    }

    return await this.domain.update(prodCode, data)
  }

  /**
   * 商品を削除
   */
  async deleteProduct(prodCode: string): Promise<void> {
    const existing = await this.domain.findById(prodCode)
    if (!existing) {
      throw new Error('商品が見つかりません')
    }

    await this.domain.delete(prodCode)
  }
}
