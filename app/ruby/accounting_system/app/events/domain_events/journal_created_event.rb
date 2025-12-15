# frozen_string_literal: true

module DomainEvents
  class JournalCreatedEvent < BaseEvent
    def initialize(journal_id:, journal_date:, description:, journal_data:, user_id:, user_name:, ip_address:)
      super(
        journal_id: journal_id,
        journal_date: journal_date,
        description: description,
        journal_data: journal_data,
        user_id: user_id,
        user_name: user_name,
        ip_address: ip_address
      )
    end

    def event_type
      'journal.created'
    end
  end
end
