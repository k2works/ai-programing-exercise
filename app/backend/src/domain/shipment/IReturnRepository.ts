import { Return } from './Return';

export interface IReturnRepository {
  findById(id: number): Promise<Return | null>;
  findByOrderId(orderId: number): Promise<Return[]>;
  save(returnEntity: Return): Promise<void>;
}
