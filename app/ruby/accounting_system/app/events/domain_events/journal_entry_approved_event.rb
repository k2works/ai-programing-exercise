# frozen_string_literal: true

module DomainEvents
  # 仕訳承認イベント
  class JournalEntryApprovedEvent
    attr_reader :journal_entry_id, :approved_by, :approval_comment, :occurred_at

    def initialize(journal_entry_id:, approved_by:, approval_comment:, occurred_at:)
      @journal_entry_id = journal_entry_id
      @approved_by = approved_by
      @approval_comment = approval_comment
      @occurred_at = occurred_at
    end

    def to_h
      {
        journal_entry_id: @journal_entry_id,
        approved_by: @approved_by,
        approval_comment: @approval_comment,
        occurred_at: @occurred_at.iso8601
      }
    end

    def to_json(*_args)
      to_h.to_json
    end
  end
end
