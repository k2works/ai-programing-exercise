import { describe, it, expect, vi } from 'vitest';
import { PlacementOrderService } from './PlacementOrderService';
import { IPlacementOrderRepository } from '../../domain/procurement/IPlacementOrderRepository';
import { IItemRepository } from '../../domain/inventory/IItemRepository';
import { Item } from '../../domain/inventory/Item';
import { PlacementOrder, PlacementOrderLine } from '../../domain/procurement/PlacementOrder';

describe('PlacementOrderService', () => {
  const mockOrderRepo: IPlacementOrderRepository = {
    findById: vi.fn(),
    findBySupplierId: vi.fn(),
    save: vi.fn(),
  };

  const mockItemRepo: IItemRepository = {
    findById: vi.fn(),
    findByCode: vi.fn(),
    save: vi.fn(),
  };

  const service = new PlacementOrderService(mockOrderRepo, mockItemRepo);

  describe('createPlacementOrder', () => {
    it('should create a placement order', async () => {
      const item = Item.create(1, 'ITEM001', 'Rose', 1, 7, 3, 10, 100, 'staff1');
      vi.mocked(mockItemRepo.findById).mockResolvedValue(item);

      const order = await service.createPlacementOrder(
        1,
        new Date('2025-01-01'),
        1,
        new Date('2025-01-10'),
        [{ itemId: 1, orderQty: 100 }],
        'staff1'
      );

      expect(order.getId()).toBe(1);
      expect(mockOrderRepo.save).toHaveBeenCalled();
    });

    it('should throw error if order quantity is not multiple of purchase unit', async () => {
      const item = Item.create(1, 'ITEM001', 'Rose', 1, 7, 3, 10, 100, 'staff1');
      vi.mocked(mockItemRepo.findById).mockResolvedValue(item);

      await expect(
        service.createPlacementOrder(
          1,
          new Date('2025-01-01'),
          1,
          new Date('2025-01-10'),
          [{ itemId: 1, orderQty: 15 }],
          'staff1'
        )
      ).rejects.toThrow('発注数量は購入単位数量(10)の倍数である必要があります');
    });
  });

  describe('cancelPlacementOrder', () => {
    it('should cancel the order', async () => {
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

      await service.cancelPlacementOrder(1);

      expect(mockOrderRepo.save).toHaveBeenCalled();
    });

    it('should throw error if order not found', async () => {
      vi.mocked(mockOrderRepo.findById).mockResolvedValue(null);

      await expect(service.cancelPlacementOrder(999)).rejects.toThrow('発注が見つかりません');
    });
  });
});
