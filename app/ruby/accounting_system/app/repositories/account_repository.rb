# frozen_string_literal: true

# インフラストラクチャ層：Output Adapter
# ActiveRecord を使用してデータアクセスを実装
class AccountRepository
  include AccountRepositoryInterface

  # すべての勘定科目を取得
  # @return [Array<Account>]
  def find_all
    Account.order(:display_order, :code).all.to_a
  end

  # 科目コードで勘定科目を検索
  # @param account_code [String] 科目コード
  # @return [Account, nil]
  def find_by_code(account_code)
    Account.find_by(code: account_code)
  end

  # BSPL 区分で勘定科目を検索
  # @param bspl_type [String] BSPL 区分（'B' または 'P'）
  # @return [Array<Account>]
  def find_by_bspl_type(bspl_type)
    Account.where(bspl_type: bspl_type)
           .order(:display_order, :code)
           .to_a
  end

  # 勘定科目を保存（作成または更新）
  # @param account [Account] 勘定科目
  # @return [Account]
  def save(account)
    account.save!
    account
  end

  # 勘定科目を削除
  # @param account_code [String] 科目コード
  # @return [void]
  def delete_by_code(account_code)
    account = find_by_code(account_code)
    account&.destroy!
  end
end
