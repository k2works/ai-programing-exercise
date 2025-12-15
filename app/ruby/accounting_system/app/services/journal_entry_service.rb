# frozen_string_literal: true

# アプリケーション層：Application Service
# 仕訳のビジネスロジックを実装
class JournalEntryService
  # @param journal_entry_repository [JournalEntryRepositoryInterface] リポジトリ
  # @param account_repository [AccountRepositoryInterface] 勘定科目リポジトリ
  def initialize(journal_entry_repository: JournalEntryRepository.new,
                 account_repository: AccountRepository.new)
    @journal_entry_repository = journal_entry_repository
    @account_repository = account_repository
  end

  # すべての仕訳を取得
  # @return [Array<JournalEntry>]
  def all_journal_entries
    @journal_entry_repository.find_all
  end

  # IDで仕訳を取得
  # @param journal_id [Integer] 仕訳ID
  # @return [JournalEntry]
  # @raise [JournalEntryNotFoundError] 仕訳が見つからない場合
  def find_journal_entry(journal_id)
    entry = @journal_entry_repository.find_by_id(journal_id)
    raise JournalEntryNotFoundError, "仕訳ID #{journal_id} が見つかりません" if entry.nil?

    entry
  end

  # 新しい仕訳を作成
  # @param attributes [Hash] 仕訳の属性
  # @return [JournalEntry]
  # @raise [InvalidJournalEntryError] 仕訳が不正な場合
  # @raise [AccountNotFoundError] 勘定科目が存在しない場合
  def create_journal_entry(attributes)
    journal_entry = JournalEntry.new(attributes)

    # ビジネスルール検証
    validate_journal_entry(journal_entry)

    # トランザクション内で保存
    ActiveRecord::Base.transaction do
      @journal_entry_repository.save(journal_entry)
    end

    journal_entry
  end

  # 仕訳を削除
  # @param journal_id [Integer] 仕訳ID
  # @return [void]
  # @raise [JournalEntryNotFoundError] 仕訳が見つからない場合
  def delete_journal_entry(journal_id)
    # 存在チェック
    find_journal_entry(journal_id)

    # トランザクション内で削除
    ActiveRecord::Base.transaction do
      @journal_entry_repository.delete_by_id(journal_id)
    end
  end

  private

  # ビジネスルール検証
  # @param entry [JournalEntry] 仕訳
  # @raise [InvalidJournalEntryError] 検証エラー
  # @raise [AccountNotFoundError] 勘定科目が存在しない
  def validate_journal_entry(entry)
    # 1. 明細が2件以上あることを確認
    if entry.details.size < 2
      raise InvalidJournalEntryError, '仕訳明細は2件以上必要です'
    end

    # 2. 勘定科目の存在確認
    entry.details.each do |detail|
      account = @account_repository.find_by_code(detail.account_code)
      raise AccountNotFoundError, "勘定科目が存在しません: #{detail.account_code}" if account.nil?
    end

    # 3. 貸借一致の検証
    unless entry.balanced?
      raise InvalidJournalEntryError, '貸借が一致していません'
    end
  end
end

# カスタム例外
class JournalEntryNotFoundError < StandardError; end
class InvalidJournalEntryError < StandardError; end
