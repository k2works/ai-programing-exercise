import { Arrival } from './Arrival';

export interface IArrivalRepository {
  findById(id: number): Promise<Arrival | null>;
  findByPlacementOrderId(placementOrderId: number): Promise<Arrival | null>;
  save(arrival: Arrival): Promise<void>;
}
