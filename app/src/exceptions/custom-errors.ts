// カスタム例外クラス
export class InvalidTypeError extends Error {
  constructor(type: number) {
    super(`無効なタイプです: ${type}`);
    this.name = 'InvalidTypeError';
  }
}

export class IndexOutOfRangeError extends Error {
  constructor(index: number, length: number) {
    super(`インデックスが範囲外です: ${index} (配列サイズ: ${length})`);
    this.name = 'IndexOutOfRangeError';
  }
}
