import { Shipment } from './Shipment';

export interface IShipmentRepository {
  findById(id: number): Promise<Shipment | null>;
  findByReceivedOrderId(receivedOrderId: number): Promise<Shipment | null>;
  save(shipment: Shipment): Promise<void>;
}
