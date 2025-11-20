export class Shipment {
  private constructor(
    private readonly id: number,
    private readonly receivedOrderId: number,
    private readonly shipmentDate: Date,
    private readonly carrier: string | null,
    private readonly trackingNumber: string | null,
    private readonly createdBy: string,
    private readonly createdAt: Date,
    private updatedAt: Date
  ) {}

  static create(
    id: number,
    receivedOrderId: number,
    shipmentDate: Date,
    scheduledShipmentDate: Date,
    carrier: string | null,
    trackingNumber: string | null,
    createdBy: string
  ): Shipment {
    // Validate shipment date is not before scheduled date
    if (shipmentDate < scheduledShipmentDate) {
      throw new Error('出荷日は予定出荷日以降である必要があります');
    }

    return new Shipment(
      id,
      receivedOrderId,
      shipmentDate,
      carrier,
      trackingNumber,
      createdBy,
      new Date(),
      new Date()
    );
  }

  static reconstruct(
    id: number,
    receivedOrderId: number,
    shipmentDate: Date,
    carrier: string | null,
    trackingNumber: string | null,
    createdBy: string,
    createdAt: Date,
    updatedAt: Date
  ): Shipment {
    return new Shipment(
      id,
      receivedOrderId,
      shipmentDate,
      carrier,
      trackingNumber,
      createdBy,
      createdAt,
      updatedAt
    );
  }

  getId(): number {
    return this.id;
  }

  getReceivedOrderId(): number {
    return this.receivedOrderId;
  }

  getShipmentDate(): Date {
    return this.shipmentDate;
  }

  getCarrier(): string | null {
    return this.carrier;
  }

  getTrackingNumber(): string | null {
    return this.trackingNumber;
  }

  getCreatedBy(): string {
    return this.createdBy;
  }

  getCreatedAt(): Date {
    return this.createdAt;
  }

  getUpdatedAt(): Date {
    return this.updatedAt;
  }

  toJSON() {
    return {
      id: this.id,
      receivedOrderId: this.receivedOrderId,
      shipmentDate: this.shipmentDate.toISOString(),
      carrier: this.carrier,
      trackingNumber: this.trackingNumber,
      createdBy: this.createdBy,
      createdAt: this.createdAt.toISOString(),
      updatedAt: this.updatedAt.toISOString(),
    };
  }
}
