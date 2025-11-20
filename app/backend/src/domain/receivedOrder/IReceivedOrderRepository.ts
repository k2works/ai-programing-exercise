import { ReceivedOrder } from './ReceivedOrder';

export interface IReceivedOrderRepository {
  findById(id: number): Promise<ReceivedOrder | null>;
  findByOrderId(orderId: number): Promise<ReceivedOrder | null>;
  save(receivedOrder: ReceivedOrder): Promise<void>;
}
