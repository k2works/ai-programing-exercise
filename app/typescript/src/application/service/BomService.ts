import { BomUseCase, CreateBomCommand, UpdateBomCommand } from '../port/in/BomUseCase';
import { BomRepository } from '../port/out/BomRepository';
import { Bom, BomExplosion } from '../../domain/model/bom/Bom';
import { DomainException } from '../../domain/exception/DomainException';

/**
 * BOM Application Service
 * Use Caseを実装し、ビジネスロジックを調整
 */
export class BomService implements BomUseCase {
  constructor(private readonly bomRepository: BomRepository) {}

  async createBom(command: CreateBomCommand): Promise<Bom> {
    // ドメインモデルを作成
    const bom = Bom.create({
      親品目コード: command.親品目コード,
      子品目コード: command.子品目コード,
      適用開始日: command.適用開始日 || new Date(),
      基準量: command.基準量,
      必要量: command.必要量,
      適用停止日: command.適用停止日,
      不良率: command.不良率,
      工順: command.工順,
    });

    // 循環参照チェック（簡易版）
    if (bom.親品目コード === bom.子品目コード) {
      throw new DomainException('親品目と子品目に同じ品目を指定することはできません');
    }

    return await this.bomRepository.save(bom);
  }

  async updateBom(command: UpdateBomCommand): Promise<Bom> {
    // 既存のBOMを削除して新しいBOMを作成（更新）
    await this.bomRepository.delete(
      command.親品目コード,
      command.子品目コード,
      command.適用開始日
    );

    const bom = Bom.create({
      親品目コード: command.親品目コード,
      子品目コード: command.子品目コード,
      適用開始日: command.適用開始日,
      基準量: command.基準量!,
      必要量: command.必要量!,
      適用停止日: command.適用停止日,
      不良率: command.不良率,
      工順: command.工順,
    });

    return await this.bomRepository.save(bom);
  }

  async getChildren(parentItemCode: string): Promise<Bom[]> {
    return await this.bomRepository.findChildren(parentItemCode);
  }

  async explodeBom(itemCode: string, quantity: number = 1): Promise<BomExplosion[]> {
    if (quantity <= 0) {
      throw new DomainException('数量は0より大きい値である必要があります');
    }
    return await this.bomRepository.explode(itemCode, quantity);
  }

  async findWherUsed(childItemCode: string): Promise<Bom[]> {
    return await this.bomRepository.findParents(childItemCode);
  }

  async deleteBom(
    parentItemCode: string,
    childItemCode: string,
    effectiveFrom: Date
  ): Promise<void> {
    await this.bomRepository.delete(parentItemCode, childItemCode, effectiveFrom);
  }
}
