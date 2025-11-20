import { describe, it, expect, vi } from 'vitest';
import { ArrivalService } from './ArrivalService';
import { IArrivalRepository } from '../../domain/procurement/IArrivalRepository';
import { IPlacementOrderRepository } from '../../domain/procurement/IPlacementOrderRepository';
import { IInventoryRepository } from '../../domain/inventory/IInventoryRepository';
import { IItemRepository } from '../../domain/inventory/IItemRepository';
import { PlacementOrder, PlacementOrderLine } from '../../domain/procurement/PlacementOrder';
import { Arrival, ArrivalLine } from '../../domain/procurement/Arrival';
import { Item } from '../../domain/inventory/Item';

describe('ArrivalService', () => {
  const mockArrivalRepo: IArrivalRepository = {
    findById: vi.fn(),
    findByPlacementOrderId: vi.fn(),
    save: vi.fn(),
  };

  const mockOrderRepo: IPlacementOrderRepository = {
    findById: vi.fn(),
    findBySupplierId: vi.fn(),
    save: vi.fn(),
  };

  const mockInventoryRepo: IInventoryRepository = {
    findByItemId: vi.fn(),
    findAvailableLots: vi.fn(),
    save: vi.fn(),
  };

  const mockItemRepo: IItemRepository = {
    findById: vi.fn(),
    findByCode: vi.fn(),
    save: vi.fn(),
  };

  const service = new ArrivalService(mockArrivalRepo, mockOrderRepo, mockInventoryRepo, mockItemRepo);

  describe('receiveItems', () => {
    it('should record arrival', async () => {
      const lines = [new PlacementOrderLine(1, 1, 100, 1000)];
      const order = PlacementOrder.create(
        1,
        new Date('2025-01-01'),
        1,
        new Date('2025-01-10'),
        lines,
        3,
        'staff1'
      );
      vi.mocked(mockOrderRepo.findById).mockResolvedValue(order);

      const arrival = await service.receiveItems(
        1,
        1,
        new Date('2025-01-10'),
        [{ placementOrderLineId: 1, itemId: 1, arrivedQty: 100 }],
        'staff1'
      );

      expect(arrival.getId()).toBe(1);
      expect(mockArrivalRepo.save).toHaveBeenCalled();
    });

    it('should throw error if placement order not found', async () => {
      vi.mocked(mockOrderRepo.findById).mockResolvedValue(null);

      await expect(
        service.receiveItems(1, 999, new Date('2025-01-10'), [], 'staff1')
      ).rejects.toThrow('発注が見つかりません');
    });
  });

  describe('inspectItems', () => {
    it('should inspect items and add to inventory', async () => {
      const lines = [new ArrivalLine(1, 1, 1, 100)];
      const arrival = Arrival.create(1, 1, new Date('2025-01-10'), lines, 'staff1');
      const item = Item.create(1, 'ITEM001', 'Rose', 1, 7, 3, 10, 100, 'staff1');

      vi.mocked(mockArrivalRepo.findById).mockResolvedValue(arrival);
      vi.mocked(mockItemRepo.findById).mockResolvedValue(item);

      await service.inspectItems(1, [{ lineId: 1, result: 'accepted' }]);

      expect(mockArrivalRepo.save).toHaveBeenCalled();
      expect(mockInventoryRepo.save).toHaveBeenCalled();
    });

    it('should throw error if arrival not found', async () => {
      vi.mocked(mockArrivalRepo.findById).mockResolvedValue(null);

      await expect(service.inspectItems(999, [])).rejects.toThrow('入荷が見つかりません');
    });
  });
});
