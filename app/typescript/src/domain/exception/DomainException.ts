/**
 * ドメイン例外基底クラス
 * ビジネスルール違反を表す
 */
export class DomainException extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'DomainException';
    Object.setPrototypeOf(this, DomainException.prototype);
  }
}

/**
 * 品目が見つからない
 */
export class ItemNotFoundException extends DomainException {
  constructor(itemCode: string) {
    super(`品目が見つかりません: ${itemCode}`);
    this.name = 'ItemNotFoundException';
    Object.setPrototypeOf(this, ItemNotFoundException.prototype);
  }
}

/**
 * 品目が重複
 */
export class DuplicateItemException extends DomainException {
  constructor(itemCode: string) {
    super(`品目コード ${itemCode} は既に使用されています`);
    this.name = 'DuplicateItemException';
    Object.setPrototypeOf(this, DuplicateItemException.prototype);
  }
}

/**
 * 在庫不足
 */
export class InsufficientInventoryException extends DomainException {
  constructor(itemCode: string, required: number, available: number) {
    super(`在庫不足: ${itemCode} (必要数: ${required}, 在庫数: ${available})`);
    this.name = 'InsufficientInventoryException';
    Object.setPrototypeOf(this, InsufficientInventoryException.prototype);
  }
}

/**
 * 無効な数量
 */
export class InvalidQuantityException extends DomainException {
  constructor(message: string) {
    super(message);
    this.name = 'InvalidQuantityException';
    Object.setPrototypeOf(this, InvalidQuantityException.prototype);
  }
}

/**
 * BOM 循環参照
 */
export class BomCircularReferenceException extends DomainException {
  constructor(itemCode: string) {
    super(`BOM に循環参照があります: ${itemCode}`);
    this.name = 'BomCircularReferenceException';
    Object.setPrototypeOf(this, BomCircularReferenceException.prototype);
  }
}
