/**
 * 品目ドメインモデル
 */
export class Item {
  品目コード: string;
  適用開始日: Date;
  品名: string;
  品目区分: string;
  品目グループコード?: string;
  単位コード?: string;
  場所コード?: string;
  リードタイム?: number;
  安全在庫数?: number;

  private constructor(params: {
    品目コード: string;
    適用開始日: Date;
    品名: string;
    品目区分: string;
    品目グループコード?: string;
    単位コード?: string;
    場所コード?: string;
    リードタイム?: number;
    安全在庫数?: number;
  }) {
    this.品目コード = params.品目コード;
    this.適用開始日 = params.適用開始日;
    this.品名 = params.品名;
    this.品目区分 = params.品目区分;
    this.品目グループコード = params.品目グループコード;
    this.単位コード = params.単位コード;
    this.場所コード = params.場所コード;
    this.リードタイム = params.リードタイム;
    this.安全在庫数 = params.安全在庫数;
  }

  /**
   * 品目を作成
   */
  static create(params: {
    品目コード: string;
    適用開始日: Date;
    品名: string;
    品目区分: string;
    品目グループコード?: string;
    単位コード?: string;
    場所コード?: string;
    リードタイム?: number;
    安全在庫数?: number;
  }): Item {
    return new Item(params);
  }
}
