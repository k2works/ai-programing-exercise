# frozen_string_literal: true

module DomainEvents
  class AccountUpdatedEvent < BaseEvent
    def initialize(account_code:, old_values:, new_values:, user_id:, user_name:, ip_address:)
      super(
        account_code: account_code,
        old_values: old_values,
        new_values: new_values,
        user_id: user_id,
        user_name: user_name,
        ip_address: ip_address
      )
    end

    def event_type
      'account.updated'
    end
  end
end
