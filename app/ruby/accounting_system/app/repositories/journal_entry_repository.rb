# frozen_string_literal: true

# インフラストラクチャ層：Output Adapter
# ActiveRecord を使用して仕訳データアクセスを実装
class JournalEntryRepository
  include JournalEntryRepositoryInterface

  # すべての仕訳を取得
  # @return [Array<JournalEntry>]
  def find_all
    JournalEntry.includes(:details).ordered.to_a
  end

  # IDで仕訳を検索
  # @param journal_id [Integer] 仕訳ID
  # @return [JournalEntry, nil]
  def find_by_id(journal_id)
    JournalEntry.includes(:details).find_by(id: journal_id)
  end

  # 仕訳を保存（作成または更新）
  # @param journal_entry [JournalEntry] 仕訳
  # @return [JournalEntry]
  def save(journal_entry)
    ActiveRecord::Base.transaction do
      journal_entry.save!
    end
    journal_entry
  end

  # 仕訳を削除
  # @param journal_id [Integer] 仕訳ID
  # @return [void]
  def delete_by_id(journal_id)
    journal_entry = find_by_id(journal_id)
    journal_entry&.destroy!
  end
end
