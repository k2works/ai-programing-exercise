import { PlacementOrder } from './PlacementOrder';

export interface IPlacementOrderRepository {
  findById(id: number): Promise<PlacementOrder | null>;
  findBySupplierId(supplierId: number): Promise<PlacementOrder[]>;
  save(order: PlacementOrder): Promise<void>;
}
