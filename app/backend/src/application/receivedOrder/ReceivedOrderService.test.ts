import { describe, it, expect, vi } from 'vitest';
import { ReceivedOrderService } from './ReceivedOrderService';
import { IReceivedOrderRepository } from '../../domain/receivedOrder/IReceivedOrderRepository';
import { IOrderRepository } from '../../domain/order/IOrderRepository';
import { IProductRepository } from '../../domain/product/IProductRepository';
import { AllocationService } from './AllocationService';
import { Order } from '../../domain/order/Order';
import { Product } from '../../domain/product/Product';

describe('ReceivedOrderService', () => {
  const mockReceivedOrderRepo: IReceivedOrderRepository = {
    findById: vi.fn(),
    findByOrderId: vi.fn(),
    save: vi.fn(),
  };

  const mockOrderRepo: IOrderRepository = {
    findById: vi.fn(),
    findByCustomerId: vi.fn(),
    findPendingOrders: vi.fn(),
    save: vi.fn(),
  };

  const mockProductRepo: IProductRepository = {
    findById: vi.fn(),
    findByCode: vi.fn(),
    findOnSale: vi.fn(),
    save: vi.fn(),
  };

  const mockAllocationService = {
    allocateInventory: vi.fn(),
  } as unknown as AllocationService;

  const service = new ReceivedOrderService(
    mockReceivedOrderRepo,
    mockOrderRepo,
    mockProductRepo,
    mockAllocationService
  );

  it('should confirm order successfully', async () => {
    const order = Order.create(
      1,
      new Date('2025-01-01'),
      1,
      1,
      2,
      new Date('2025-01-05'),
      'Test Address',
      '000-0000-0000',
      null,
      'customer1'
    );

    const product = Product.create(1, 'PRD001', 'Test Product', 3000, 'staff1');

    vi.mocked(mockOrderRepo.findById).mockResolvedValue(order);
    vi.mocked(mockReceivedOrderRepo.findByOrderId).mockResolvedValue(null);
    vi.mocked(mockProductRepo.findById).mockResolvedValue(product);
    vi.mocked(mockAllocationService.allocateInventory).mockResolvedValue(true);

    const result = await service.confirmOrder(1, 'staff1');

    expect(result.getOrderId()).toBe(1);
    expect(result.getAllocationStatus()).toBe('allocated');
    expect(result.getTotalAmount()).toBe(6000); // 3000 * 2
    expect(mockReceivedOrderRepo.save).toHaveBeenCalled();
  });

  it('should throw error when order not found', async () => {
    vi.mocked(mockOrderRepo.findById).mockResolvedValue(null);

    await expect(service.confirmOrder(999, 'staff1')).rejects.toThrow('注文が見つかりません');
  });

  it('should throw error when already confirmed', async () => {
    const order = Order.create(
      1,
      new Date('2025-01-01'),
      1,
      1,
      2,
      new Date('2025-01-05'),
      'Test Address',
      '000-0000-0000',
      null,
      'customer1'
    );

    vi.mocked(mockOrderRepo.findById).mockResolvedValue(order);
    vi.mocked(mockReceivedOrderRepo.findByOrderId).mockResolvedValue({} as any);

    await expect(service.confirmOrder(1, 'staff1')).rejects.toThrow('既に受注確認済みです');
  });

  it('should throw error when insufficient inventory', async () => {
    const order = Order.create(
      1,
      new Date('2025-01-01'),
      1,
      1,
      2,
      new Date('2025-01-05'),
      'Test Address',
      '000-0000-0000',
      null,
      'customer1'
    );

    const product = Product.create(1, 'PRD001', 'Test Product', 3000, 'staff1');

    vi.mocked(mockOrderRepo.findById).mockResolvedValue(order);
    vi.mocked(mockReceivedOrderRepo.findByOrderId).mockResolvedValue(null);
    vi.mocked(mockProductRepo.findById).mockResolvedValue(product);
    vi.mocked(mockAllocationService.allocateInventory).mockResolvedValue(false);

    await expect(service.confirmOrder(1, 'staff1')).rejects.toThrow('在庫が不足しています');
  });
});
