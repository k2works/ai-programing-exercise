import { Bom, BomExplosion } from '../../../domain/model/bom/Bom';

/**
 * BOM Repository (Output Port)
 * Infrastructure層が実装するインターフェース
 */
export interface BomRepository {
  /**
   * BOM情報を保存
   */
  save(bom: Bom): Promise<Bom>;

  /**
   * 親品目コードで子品目を取得
   */
  findChildren(parentItemCode: string): Promise<Bom[]>;

  /**
   * BOMを展開（部品展開）
   * @param itemCode 品目コード
   * @param quantity 数量
   */
  explode(itemCode: string, quantity: number): Promise<BomExplosion[]>;

  /**
   * 子品目コードで使用先を検索（逆引き）
   */
  findParents(childItemCode: string): Promise<Bom[]>;

  /**
   * BOM情報を削除
   */
  delete(parentItemCode: string, childItemCode: string, effectiveFrom: Date): Promise<void>;
}
