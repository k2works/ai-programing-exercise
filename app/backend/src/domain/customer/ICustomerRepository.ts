import { Customer } from './Customer';

export interface ICustomerRepository {
  findById(id: number): Promise<Customer | null>;
  findByCode(code: string): Promise<Customer | null>;
  save(customer: Customer): Promise<void>;
}
