# frozen_string_literal: true

# アプリケーション層：Output Port
# 仕訳データアクセスの抽象化
module JournalEntryRepositoryInterface
  # すべての仕訳を取得
  # @return [Array<JournalEntry>] 仕訳リスト
  def find_all
    raise NotImplementedError, "#{self.class}#find_all must be implemented"
  end

  # IDで仕訳を検索
  # @param journal_id [Integer] 仕訳ID
  # @return [JournalEntry, nil] 仕訳（存在しない場合は nil）
  def find_by_id(journal_id)
    raise NotImplementedError, "#{self.class}#find_by_id must be implemented"
  end

  # 仕訳を保存（作成または更新）
  # @param journal_entry [JournalEntry] 仕訳
  # @return [JournalEntry] 保存された仕訳
  def save(journal_entry)
    raise NotImplementedError, "#{self.class}#save must be implemented"
  end

  # 仕訳を削除
  # @param journal_id [Integer] 仕訳ID
  # @return [void]
  def delete_by_id(journal_id)
    raise NotImplementedError, "#{self.class}#delete_by_id must be implemented"
  end
end
