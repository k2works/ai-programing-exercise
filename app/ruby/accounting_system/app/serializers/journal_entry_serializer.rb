# frozen_string_literal: true

# インフラストラクチャ層：レスポンス変換
# 仕訳のシリアライザ
class JournalEntrySerializer
  def initialize(journal_entry)
    @journal_entry = journal_entry
  end

  def as_json
    {
      id: @journal_entry.id,
      entry_number: @journal_entry.entry_number,
      entry_date: @journal_entry.entry_date,
      description: @journal_entry.description,
      total_amount: @journal_entry.total_amount,
      created_by: @journal_entry.created_by,
      details: @journal_entry.details.map do |detail|
        {
          id: detail.id,
          line_number: detail.line_number,
          account_code: detail.account_code,
          debit_amount: detail.debit_amount,
          credit_amount: detail.credit_amount,
          description: detail.description
        }
      end
    }
  end
end
