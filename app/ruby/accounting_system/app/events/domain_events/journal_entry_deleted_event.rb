# frozen_string_literal: true

module DomainEvents
  # 仕訳削除イベント
  class JournalEntryDeletedEvent
    attr_reader :journal_entry_id, :deleted_by, :reason, :occurred_at

    def initialize(journal_entry_id:, deleted_by:, reason:, occurred_at:)
      @journal_entry_id = journal_entry_id
      @deleted_by = deleted_by
      @reason = reason
      @occurred_at = occurred_at
    end

    def to_h
      {
        journal_entry_id: @journal_entry_id,
        deleted_by: @deleted_by,
        reason: @reason,
        occurred_at: @occurred_at.iso8601
      }
    end

    def to_json(*_args)
      to_h.to_json
    end
  end
end
