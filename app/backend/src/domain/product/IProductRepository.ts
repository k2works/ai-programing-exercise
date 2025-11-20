import { Product } from './Product';

export interface IProductRepository {
  findById(id: number): Promise<Product | null>;
  findByCode(code: string): Promise<Product | null>;
  findOnSale(): Promise<Product[]>;
  save(product: Product): Promise<void>;
}
