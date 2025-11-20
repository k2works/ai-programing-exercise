import { Supplier } from './Supplier';

export interface ISupplierRepository {
  findById(id: number): Promise<Supplier | null>;
  findByCode(code: string): Promise<Supplier | null>;
  save(supplier: Supplier): Promise<void>;
}
