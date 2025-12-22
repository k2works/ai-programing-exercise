import { Bom, BomExplosion } from '../../../domain/model/bom/Bom';

/**
 * BOM登録コマンド
 */
export interface CreateBomCommand {
  親品目コード: string;
  子品目コード: string;
  適用開始日?: Date;
  基準量: number;
  必要量: number;
  適用停止日?: Date;
  不良率?: number;
  工順?: number;
}

/**
 * BOM更新コマンド
 */
export interface UpdateBomCommand {
  親品目コード: string;
  子品目コード: string;
  適用開始日: Date;
  基準量?: number;
  必要量?: number;
  適用停止日?: Date;
  不良率?: number;
  工順?: number;
}

/**
 * BOM Use Case (Input Port)
 * Application Serviceが実装するインターフェース
 */
export interface BomUseCase {
  /**
   * BOMを登録
   */
  createBom(command: CreateBomCommand): Promise<Bom>;

  /**
   * BOMを更新
   */
  updateBom(command: UpdateBomCommand): Promise<Bom>;

  /**
   * 親品目の子品目一覧を取得
   */
  getChildren(parentItemCode: string): Promise<Bom[]>;

  /**
   * BOMを展開（部品展開）
   */
  explodeBom(itemCode: string, quantity?: number): Promise<BomExplosion[]>;

  /**
   * 使用先照会（逆引き）
   */
  findWherUsed(childItemCode: string): Promise<Bom[]>;

  /**
   * BOMを削除
   */
  deleteBom(parentItemCode: string, childItemCode: string, effectiveFrom: Date): Promise<void>;
}
