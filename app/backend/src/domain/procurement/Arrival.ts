export type InspectionStatus = 'uninspected' | 'inspected' | 'returned';
export type InspectionResult = 'accepted' | 'rejected';

export class ArrivalLine {
  constructor(
    public readonly id: number,
    public readonly placementOrderLineId: number,
    public readonly itemId: number,
    public readonly arrivedQty: number,
    public inspectionResult: InspectionResult | null = null
  ) {}

  inspect(result: InspectionResult): void {
    this.inspectionResult = result;
  }
}

export class Arrival {
  private constructor(
    private readonly id: number,
    private readonly placementOrderId: number,
    private readonly arrivalDate: Date,
    private readonly lines: ArrivalLine[],
    private inspectionStatus: InspectionStatus,
    private readonly createdBy: string,
    private readonly createdAt: Date,
    private updatedAt: Date
  ) {}

  static create(
    id: number,
    placementOrderId: number,
    arrivalDate: Date,
    lines: ArrivalLine[],
    createdBy: string
  ): Arrival {
    return new Arrival(
      id,
      placementOrderId,
      arrivalDate,
      lines,
      'uninspected',
      createdBy,
      new Date(),
      new Date()
    );
  }

  static reconstruct(
    id: number,
    placementOrderId: number,
    arrivalDate: Date,
    lines: ArrivalLine[],
    inspectionStatus: InspectionStatus,
    createdBy: string,
    createdAt: Date,
    updatedAt: Date
  ): Arrival {
    return new Arrival(
      id,
      placementOrderId,
      arrivalDate,
      lines,
      inspectionStatus,
      createdBy,
      createdAt,
      updatedAt
    );
  }

  inspect(): void {
    if (this.inspectionStatus === 'inspected') {
      throw new Error('既に検収済みです');
    }

    // Check if all lines are inspected
    const allInspected = this.lines.every((line) => line.inspectionResult !== null);
    if (!allInspected) {
      throw new Error('全ての明細を検収してください');
    }

    // Check if any line is rejected
    const hasRejected = this.lines.some((line) => line.inspectionResult === 'rejected');
    this.inspectionStatus = hasRejected ? 'returned' : 'inspected';
    this.updatedAt = new Date();
  }

  getId(): number {
    return this.id;
  }

  getPlacementOrderId(): number {
    return this.placementOrderId;
  }

  getArrivalDate(): Date {
    return this.arrivalDate;
  }

  getLines(): ArrivalLine[] {
    return this.lines;
  }

  getInspectionStatus(): InspectionStatus {
    return this.inspectionStatus;
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
      placementOrderId: this.placementOrderId,
      arrivalDate: this.arrivalDate.toISOString(),
      inspectionStatus: this.inspectionStatus,
      lines: this.lines,
      createdBy: this.createdBy,
      createdAt: this.createdAt.toISOString(),
      updatedAt: this.updatedAt.toISOString(),
    };
  }
}
