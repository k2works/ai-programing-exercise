/**
 * MRP実行結果
 */
export interface MRPExecutionResult {
  /**
   * MPS番号
   */
  mpsNumber: string;

  /**
   * 生成された製造オーダ数
   */
  generatedManufacturingOrders: number;

  /**
   * 生成された購買オーダ数
   */
  generatedPurchaseOrders: number;

  /**
   * 生成された所要情報数
   */
  generatedRequirements: number;

  /**
   * 実行日時
   */
  executedAt: Date;
}

/**
 * MRP Use Case (Input Port)
 *
 * MRP（Material Requirements Planning：資材所要量計画）を実行するユースケース。
 * 基準生産計画（MPS）を基に、製造オーダ、購買オーダ、所要情報を生成します。
 */
export interface MRPUseCase {
  /**
   * MRPを実行する
   *
   * 処理フロー：
   * 1. MPSをCONFIRMEDステータスで取得
   * 2. MPSから製造オーダを生成
   * 3. BOMを展開して所要量を計算
   * 4. 所要情報（Requirement）を作成
   * 5. MPSステータスをEXPANDEDに更新
   *
   * @param mpsNumber MPS番号
   * @returns MRP実行結果
   * @throws {Error} MPSが見つからない、またはステータスがCONFIRMED以外の場合
   */
  execute(mpsNumber: string): Promise<MRPExecutionResult>;
}
