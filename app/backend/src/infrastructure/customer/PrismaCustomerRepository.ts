import { PrismaClient } from '@prisma/client';
import { Customer } from '../../domain/customer/Customer';
import { ICustomerRepository } from '../../domain/customer/ICustomerRepository';

export class PrismaCustomerRepository implements ICustomerRepository {
  constructor(private readonly prisma: PrismaClient) {}

  async findById(id: number): Promise<Customer | null> {
    const customerData = await this.prisma.customer.findUnique({
      where: { id },
    });

    if (!customerData) {
      return null;
    }

    return Customer.reconstruct(
      customerData.id,
      customerData.code,
      customerData.name,
      customerData.phone,
      customerData.email,
      customerData.creditCardInfo,
      customerData.createdBy,
      customerData.createdAt,
      customerData.updatedAt
    );
  }

  async findByCode(code: string): Promise<Customer | null> {
    const customerData = await this.prisma.customer.findUnique({
      where: { code },
    });

    if (!customerData) {
      return null;
    }

    return Customer.reconstruct(
      customerData.id,
      customerData.code,
      customerData.name,
      customerData.phone,
      customerData.email,
      customerData.creditCardInfo,
      customerData.createdBy,
      customerData.createdAt,
      customerData.updatedAt
    );
  }

  async save(customer: Customer): Promise<void> {
    const data = customer.toJSON();

    await this.prisma.customer.upsert({
      where: { id: customer.getId() },
      update: {
        phone: data.phone,
        email: data.email,
        creditCardInfo: data.creditCardInfo,
        updatedAt: new Date(),
      },
      create: {
        id: data.id,
        code: data.code,
        name: data.name,
        phone: data.phone,
        email: data.email,
        creditCardInfo: data.creditCardInfo,
        createdBy: data.createdBy,
      },
    });
  }
}
