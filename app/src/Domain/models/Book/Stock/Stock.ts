import { QuantityAvailable } from './QuantityAvailable/QuantityAvailable';
import { Status, StatusEnum } from './Status/Status';
import { StockId } from './StockId/StockId';

export class Stock {
  private constructor(
    private readonly _stockId: StockId,
    private _quantityAvailable: QuantityAvailable,
    private _status: Status
  ) {}

  static create() {
    const defaultStockId = new StockId();
    const defaultQuantityAvailable = new QuantityAvailable(0);
    const defaultStatus = new Status(StatusEnum.OutOfStock);

    return new Stock(defaultStockId, defaultQuantityAvailable, defaultStatus);
  }

  delete() {
    if (this.status.value !== StatusEnum.OutOfStock) {
      throw new Error('在庫がある場合削除できません。');
    }
  }

  private changeStatus(newStatus: Status) {
    this._status = newStatus;
  }

  increaseQuantity(amount: number) {
    if (amount < 0) {
      throw new Error('増加量は0以上でなければなりません。');
    }

    const newQuantity = this.quantityAvailable.increment(amount).value;

    if (newQuantity <= 10) {
      this.changeStatus(new Status(StatusEnum.LowStock));
    }
    this._quantityAvailable = new QuantityAvailable(newQuantity);
  }

  decreaseQuantity(amount: number) {
    if (amount < 0) {
      throw new Error('減少量は0以上でなければなりません。');
    }

    const newQuantity = this.quantityAvailable.decrement(amount).value;
    if (newQuantity < 0) {
      throw new Error('減少後の在庫数が0未満になってしまいます。');
    }

    if (newQuantity <= 10) {
      this.changeStatus(new Status(StatusEnum.LowStock));
    }

    if (newQuantity === 0) {
      this.changeStatus(new Status(StatusEnum.OutOfStock));
    }

    this._quantityAvailable = new QuantityAvailable(newQuantity);
  }

  static reconstruct(
    stockId: StockId,
    quantityAvailable: QuantityAvailable,
    status: Status
  ) {
    return new Stock(stockId, quantityAvailable, status);
  }

  get stockId(): StockId {
    return this._stockId;
  }

  get quantityAvailable(): QuantityAvailable {
    return this._quantityAvailable;
  }

  get status(): Status {
    return this._status;
  }
}
