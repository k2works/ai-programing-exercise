import { describe, it, expect, vi } from 'vitest';
import { InventoryManagementService } from './InventoryManagementService';
import { ISupplierRepository } from '../../domain/inventory/ISupplierRepository';
import { IItemRepository } from '../../domain/inventory/IItemRepository';
import { Supplier } from '../../domain/inventory/Supplier';
import { Item } from '../../domain/inventory/Item';

describe('InventoryManagementService', () => {
  const mockSupplierRepo: ISupplierRepository = {
    findById: vi.fn(),
    findByCode: vi.fn(),
    save: vi.fn(),
  };

  const mockItemRepo: IItemRepository = {
    findById: vi.fn(),
    findByCode: vi.fn(),
    save: vi.fn(),
  };

  const service = new InventoryManagementService(mockSupplierRepo, mockItemRepo);

  describe('registerSupplier', () => {
    it('should register a new supplier', async () => {
      vi.mocked(mockSupplierRepo.findByCode).mockResolvedValue(null);

      const supplier = await service.registerSupplier(1, 'SUP001', 'Test Supplier', null, null, 'staff1');

      expect(supplier.getCode()).toBe('SUP001');
      expect(mockSupplierRepo.save).toHaveBeenCalled();
    });

    it('should throw error if code already exists', async () => {
      const existing = Supplier.create(1, 'SUP001', 'Existing', null, null, 'staff1');
      vi.mocked(mockSupplierRepo.findByCode).mockResolvedValue(existing);

      await expect(
        service.registerSupplier(2, 'SUP001', 'Test', null, null, 'staff1')
      ).rejects.toThrow('仕入先コードが既に存在します');
    });
  });

  describe('deactivateSupplier', () => {
    it('should deactivate supplier', async () => {
      const supplier = Supplier.create(1, 'SUP001', 'Test', null, null, 'staff1');
      vi.mocked(mockSupplierRepo.findById).mockResolvedValue(supplier);

      await service.deactivateSupplier(1);

      expect(mockSupplierRepo.save).toHaveBeenCalled();
    });
  });

  describe('registerItem', () => {
    it('should register a new item', async () => {
      const supplier = Supplier.create(1, 'SUP001', 'Test', null, null, 'staff1');
      vi.mocked(mockSupplierRepo.findById).mockResolvedValue(supplier);
      vi.mocked(mockItemRepo.findByCode).mockResolvedValue(null);

      const item = await service.registerItem(1, 'ITEM001', 'Rose', 1, 7, 3, 10, 100, 'staff1');

      expect(item.getCode()).toBe('ITEM001');
      expect(mockItemRepo.save).toHaveBeenCalled();
    });

    it('should throw error if supplier not found', async () => {
      vi.mocked(mockSupplierRepo.findById).mockResolvedValue(null);

      await expect(
        service.registerItem(1, 'ITEM001', 'Rose', 999, 7, 3, 10, 100, 'staff1')
      ).rejects.toThrow('仕入先が見つかりません');
    });

    it('should throw error if item code already exists', async () => {
      const supplier = Supplier.create(1, 'SUP001', 'Test', null, null, 'staff1');
      const existing = Item.create(1, 'ITEM001', 'Existing', 1, 7, 3, 10, 100, 'staff1');
      vi.mocked(mockSupplierRepo.findById).mockResolvedValue(supplier);
      vi.mocked(mockItemRepo.findByCode).mockResolvedValue(existing);

      await expect(
        service.registerItem(2, 'ITEM001', 'Rose', 1, 7, 3, 10, 100, 'staff1')
      ).rejects.toThrow('単品コードが既に存在します');
    });
  });
});
