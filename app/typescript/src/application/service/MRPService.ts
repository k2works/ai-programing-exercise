import { MRPUseCase, MRPExecutionResult } from '../port/in/MRPUseCase';
import {
  MPSRepository,
  PlanStatus as MPSPlanStatus,
} from '../../domain/production-plan/mps.repository';
import {
  OrderRepository,
  OrderType,
  PlanStatus as OrderPlanStatus,
} from '../../domain/production-plan/order.repository';
import { RequirementRepository } from '../../domain/production-plan/requirement.repository';
import { BomRepository } from '../port/out/BomRepository';

/**
 * MRP Application Service
 *
 * MRP（資材所要量計画）の実行を管理するアプリケーションサービス。
 */
export class MRPService implements MRPUseCase {
  constructor(
    private readonly mpsRepository: MPSRepository,
    private readonly orderRepository: OrderRepository,
    private readonly requirementRepository: RequirementRepository,
    private readonly bomRepository: BomRepository
  ) {}

  async execute(mpsNumber: string): Promise<MRPExecutionResult> {
    // 1. MPSを取得
    const mps = await this.mpsRepository.findByNumber(mpsNumber);
    if (!mps) {
      throw new Error(`MPS not found: ${mpsNumber}`);
    }

    // 2. MPSステータスの検証
    if (mps.status !== MPSPlanStatus.CONFIRMED) {
      throw new Error(`MPS status must be CONFIRMED, but was: ${mps.status}`);
    }

    let generatedManufacturingOrders = 0;
    let generatedPurchaseOrders = 0;
    let generatedRequirements = 0;

    try {
      // 3. 製造オーダを生成
      const manufacturingOrder = await this.orderRepository.create({
        orderNumber: `MO-${mpsNumber}`,
        orderType: OrderType.MANUFACTURING,
        itemCode: mps.itemCode,
        startDate: mps.planDate,
        dueDate: mps.dueDate,
        plannedQuantity: Number(mps.plannedQuantity),
        locationCode: mps.locationCode || 'DEFAULT',
        status: OrderPlanStatus.CONFIRMED,
        mpsId: mps.id,
      });

      generatedManufacturingOrders++;

      // 4. BOMを展開して所要量を計算
      const bomItems = await this.bomRepository.findChildren(mps.itemCode);

      // 5. BOM明細ごとに所要情報と購買オーダを生成
      let itemIndex = 1;
      for (const bomItem of bomItems) {
        // 所要量 = BOM必要量 * MPS計画数量 / BOM基準量
        const requiredQuantity =
          (Number(bomItem.必要量) * Number(mps.plannedQuantity)) / Number(bomItem.基準量);

        // 所要情報を作成
        await this.requirementRepository.create({
          requirementNumber: `REQ-${mpsNumber}-${itemIndex.toString().padStart(3, '0')}`,
          orderId: manufacturingOrder.id,
          itemCode: bomItem.子品目コード,
          dueDate: mps.planDate, // 製造開始日に必要
          requiredQuantity,
          locationCode: mps.locationCode || 'DEFAULT',
        });

        generatedRequirements++;

        // 購買オーダを生成（簡略版：全て購買する想定）
        await this.orderRepository.create({
          orderNumber: `PO-${mpsNumber}-${itemIndex.toString().padStart(3, '0')}`,
          orderType: OrderType.PURCHASE,
          itemCode: bomItem.子品目コード,
          startDate: new Date(mps.planDate.getTime() - 7 * 24 * 60 * 60 * 1000), // 7日前
          dueDate: mps.planDate,
          plannedQuantity: requiredQuantity,
          locationCode: mps.locationCode || 'DEFAULT',
          status: OrderPlanStatus.DRAFT,
          parentOrderId: manufacturingOrder.id,
        });

        generatedPurchaseOrders++;
        itemIndex++;
      }

      // 6. MPSステータスをEXPANDEDに更新
      await this.mpsRepository.updateStatus(mps.id, MPSPlanStatus.EXPANDED);

      return {
        mpsNumber: mps.mpsNumber,
        generatedManufacturingOrders,
        generatedPurchaseOrders,
        generatedRequirements,
        executedAt: new Date(),
      };
    } catch (error) {
      // エラー時は生成途中のデータが残るが、トランザクション制御は別レイヤで実装
      throw new Error(`MRP execution failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }
}
