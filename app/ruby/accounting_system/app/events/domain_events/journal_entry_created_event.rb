# frozen_string_literal: true

module DomainEvents
  # 仕訳作成イベント
  class JournalEntryCreatedEvent
    attr_reader :journal_entry_id, :entry_date, :description, :line_items, :user_id, :occurred_at

    def initialize(journal_entry_id:, entry_date:, description:, line_items:, user_id:, occurred_at:)
      @journal_entry_id = journal_entry_id
      @entry_date = entry_date
      @description = description
      @line_items = line_items
      @user_id = user_id
      @occurred_at = occurred_at
    end

    def to_h
      {
        journal_entry_id: @journal_entry_id,
        entry_date: @entry_date.respond_to?(:iso8601) ? @entry_date.iso8601 : @entry_date.to_s,
        description: @description,
        line_items: @line_items,
        user_id: @user_id,
        occurred_at: @occurred_at.iso8601
      }
    end

    def to_json(*_args)
      to_h.to_json
    end
  end
end
