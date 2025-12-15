# frozen_string_literal: true

module DomainEvents
  class AccountCreatedEvent < BaseEvent
    def initialize(account_code:, change_data:, user_id:, user_name:, ip_address:)
      super(
        account_code: account_code,
        change_data: change_data,
        user_id: user_id,
        user_name: user_name,
        ip_address: ip_address
      )
    end

    def event_type
      'account.created'
    end
  end
end
