# frozen_string_literal: true

# アプリケーション層：Output Port
# データアクセスの抽象化
module AccountRepositoryInterface
  # すべての勘定科目を取得
  # @return [Array<Account>] 勘定科目リスト
  def find_all
    raise NotImplementedError, "#{self.class}#find_all must be implemented"
  end

  # 科目コードで勘定科目を検索
  # @param account_code [String] 科目コード
  # @return [Account, nil] 勘定科目（存在しない場合は nil）
  def find_by_code(account_code)
    raise NotImplementedError, "#{self.class}#find_by_code must be implemented"
  end

  # BSPL 区分で勘定科目を検索
  # @param bspl_type [String] BSPL 区分（'B' または 'P'）
  # @return [Array<Account>] 勘定科目リスト
  def find_by_bspl_type(bspl_type)
    raise NotImplementedError, "#{self.class}#find_by_bspl_type must be implemented"
  end

  # 勘定科目を保存（作成または更新）
  # @param account [Account] 勘定科目
  # @return [Account] 保存された勘定科目
  def save(account)
    raise NotImplementedError, "#{self.class}#save must be implemented"
  end

  # 勘定科目を削除
  # @param account_code [String] 科目コード
  # @return [void]
  def delete_by_code(account_code)
    raise NotImplementedError, "#{self.class}#delete_by_code must be implemented"
  end
end
