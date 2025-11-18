// src/application/port/out/JournalRepository.ts
import { JournalWithDetails } from '../in/JournalUseCase'

/**
 * 仕訳リポジトリ（Output Port）
 * ドメイン層がデータアクセスに依存しないためのインターフェース
 */
export interface JournalRepository {
  /**
   * 仕訳を保存する（作成または更新）
   * @param journal 保存する仕訳
   * @returns 保存された仕訳
   */
  save(journal: JournalWithDetails): Promise<JournalWithDetails>

  /**
   * すべての仕訳を取得する
   * @returns 仕訳一覧（起票日順でソート）
   */
  findAll(): Promise<JournalWithDetails[]>

  /**
   * 伝票番号で仕訳を検索する
   * @param voucherNo 伝票番号
   * @returns 仕訳、見つからない場合はnull
   */
  findByVoucherNo(voucherNo: string): Promise<JournalWithDetails | null>

  /**
   * 日付範囲で仕訳を取得する
   * @param startDate 開始日
   * @param endDate 終了日
   * @returns 仕訳一覧（起票日順でソート）
   */
  findByDateRange(startDate: Date, endDate: Date): Promise<JournalWithDetails[]>

  /**
   * 部門コードで仕訳を取得する
   * @param departmentCode 部門コード
   * @returns 仕訳一覧（起票日順でソート）
   */
  findByDepartmentCode(departmentCode: string): Promise<JournalWithDetails[]>

  /**
   * 仕訳を削除する
   * @param voucherNo 削除する仕訳の伝票番号
   */
  deleteByVoucherNo(voucherNo: string): Promise<void>

  /**
   * すべての仕訳を削除する（テスト用）
   */
  deleteAll(): Promise<void>
}
