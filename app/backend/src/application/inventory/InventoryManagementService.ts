import { ISupplierRepository } from '../../domain/inventory/ISupplierRepository';
import { IItemRepository } from '../../domain/inventory/IItemRepository';
import { Supplier } from '../../domain/inventory/Supplier';
import { Item } from '../../domain/inventory/Item';

export class InventoryManagementService {
  constructor(
    private supplierRepository: ISupplierRepository,
    private itemRepository: IItemRepository
  ) {}

  async registerSupplier(
    id: number,
    code: string,
    name: string,
    phone: string | null,
    email: string | null,
    createdBy: string
  ): Promise<Supplier> {
    const existing = await this.supplierRepository.findByCode(code);
    if (existing) {
      throw new Error('仕入先コードが既に存在します');
    }

    const supplier = Supplier.create(id, code, name, phone, email, createdBy);
    await this.supplierRepository.save(supplier);
    return supplier;
  }

  async updateSupplier(id: number, phone: string | null, email: string | null): Promise<void> {
    const supplier = await this.supplierRepository.findById(id);
    if (!supplier) {
      throw new Error('仕入先が見つかりません');
    }

    // Note: Supplier entity would need update methods for phone/email
    // For now, we reconstruct with updated values
    const updated = Supplier.reconstruct(
      supplier.getId(),
      supplier.getCode(),
      supplier.getName(),
      phone,
      email,
      supplier.getStatus(),
      supplier.getCreatedBy(),
      supplier.getCreatedAt(),
      new Date()
    );

    await this.supplierRepository.save(updated);
  }

  async deactivateSupplier(id: number): Promise<void> {
    const supplier = await this.supplierRepository.findById(id);
    if (!supplier) {
      throw new Error('仕入先が見つかりません');
    }

    supplier.deactivate();
    await this.supplierRepository.save(supplier);
  }

  async activateSupplier(id: number): Promise<void> {
    const supplier = await this.supplierRepository.findById(id);
    if (!supplier) {
      throw new Error('仕入先が見つかりません');
    }

    supplier.activate();
    await this.supplierRepository.save(supplier);
  }

  async registerItem(
    id: number,
    code: string,
    name: string,
    supplierId: number,
    qualityDays: number,
    leadTime: number,
    purchaseUnitQty: number,
    purchasePrice: number,
    createdBy: string
  ): Promise<Item> {
    const supplier = await this.supplierRepository.findById(supplierId);
    if (!supplier) {
      throw new Error('仕入先が見つかりません');
    }

    const existing = await this.itemRepository.findByCode(code);
    if (existing) {
      throw new Error('単品コードが既に存在します');
    }

    const item = Item.create(
      id,
      code,
      name,
      supplierId,
      qualityDays,
      leadTime,
      purchaseUnitQty,
      purchasePrice,
      createdBy
    );
    await this.itemRepository.save(item);
    return item;
  }

  async updateItem(
    id: number,
    qualityDays: number,
    leadTime: number,
    purchaseUnitQty: number,
    purchasePrice: number
  ): Promise<void> {
    const item = await this.itemRepository.findById(id);
    if (!item) {
      throw new Error('単品が見つかりません');
    }

    const updated = Item.reconstruct(
      item.getId(),
      item.getCode(),
      item.getName(),
      item.getSupplierId(),
      qualityDays,
      leadTime,
      purchaseUnitQty,
      purchasePrice,
      item.getCreatedBy(),
      item.getCreatedAt(),
      new Date()
    );

    await this.itemRepository.save(updated);
  }
}
