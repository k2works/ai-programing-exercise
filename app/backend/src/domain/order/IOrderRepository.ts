import { Order } from './Order';

export interface IOrderRepository {
  findById(id: number): Promise<Order | null>;
  findByCustomerId(customerId: number): Promise<Order[]>;
  findPendingOrders(): Promise<Order[]>;
  save(order: Order): Promise<void>;
}
