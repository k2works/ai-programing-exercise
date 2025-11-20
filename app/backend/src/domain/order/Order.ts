export class Order {
  private constructor(
    private readonly id: number,
    private readonly orderDate: Date,
    private readonly customerId: number,
    private readonly productId: number,
    private readonly quantity: number,
    private desiredDeliveryDate: Date,
    private readonly deliveryAddress: string,
    private readonly deliveryPhone: string,
    private readonly deliveryMessage: string | null,
    private status: string,
    private readonly createdBy: string,
    private readonly createdAt: Date,
    private updatedAt: Date
  ) {}

  static create(
    id: number,
    orderDate: Date,
    customerId: number,
    productId: number,
    quantity: number,
    desiredDeliveryDate: Date,
    deliveryAddress: string,
    deliveryPhone: string,
    deliveryMessage: string | null,
    createdBy: string
  ): Order {
    // Validate delivery date is at least 2 days after order date
    const minDeliveryDate = new Date(orderDate);
    minDeliveryDate.setDate(minDeliveryDate.getDate() + 2);
    
    if (desiredDeliveryDate < minDeliveryDate) {
      throw new Error('配送日は注文日の2日後以降である必要があります');
    }

    return new Order(
      id,
      orderDate,
      customerId,
      productId,
      quantity,
      desiredDeliveryDate,
      deliveryAddress,
      deliveryPhone,
      deliveryMessage,
      'pending',
      createdBy,
      new Date(),
      new Date()
    );
  }

  static reconstruct(
    id: number,
    orderDate: Date,
    customerId: number,
    productId: number,
    quantity: number,
    desiredDeliveryDate: Date,
    deliveryAddress: string,
    deliveryPhone: string,
    deliveryMessage: string | null,
    status: string,
    createdBy: string,
    createdAt: Date,
    updatedAt: Date
  ): Order {
    return new Order(
      id,
      orderDate,
      customerId,
      productId,
      quantity,
      desiredDeliveryDate,
      deliveryAddress,
      deliveryPhone,
      deliveryMessage,
      status,
      createdBy,
      createdAt,
      updatedAt
    );
  }

  changeDeliveryDate(newDate: Date): void {
    if (this.status === 'shipped' || this.status === 'delivered') {
      throw new Error('出荷済みの注文は変更できません');
    }

    this.desiredDeliveryDate = newDate;
    this.updatedAt = new Date();
  }

  cancel(): void {
    if (this.status === 'shipped' || this.status === 'delivered') {
      throw new Error('出荷済みの注文はキャンセルできません');
    }

    this.status = 'cancelled';
    this.updatedAt = new Date();
  }

  getShipmentDate(): Date {
    const shipmentDate = new Date(this.desiredDeliveryDate);
    shipmentDate.setDate(shipmentDate.getDate() - 2);
    return shipmentDate;
  }

  getId(): number {
    return this.id;
  }

  getOrderDate(): Date {
    return this.orderDate;
  }

  getCustomerId(): number {
    return this.customerId;
  }

  getProductId(): number {
    return this.productId;
  }

  getQuantity(): number {
    return this.quantity;
  }

  getDesiredDeliveryDate(): Date {
    return this.desiredDeliveryDate;
  }

  getStatus(): string {
    return this.status;
  }

  toJSON() {
    return {
      id: this.id,
      orderDate: this.orderDate,
      customerId: this.customerId,
      productId: this.productId,
      quantity: this.quantity,
      desiredDeliveryDate: this.desiredDeliveryDate,
      deliveryAddress: this.deliveryAddress,
      deliveryPhone: this.deliveryPhone,
      deliveryMessage: this.deliveryMessage,
      status: this.status,
      createdBy: this.createdBy,
      createdAt: this.createdAt,
      updatedAt: this.updatedAt,
    };
  }
}
