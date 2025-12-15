# frozen_string_literal: true

FactoryBot.define do
  factory :audit_log do
    entity_type { 'Account' }
    entity_id { '1001' }
    action { :create }
    user_id { 'user001' }
    user_name { '山田太郎' }
    timestamp { Time.current }
    change_data { { account_name: '現金' } }
    ip_address { '192.168.1.100' }

    trait :update do
      action { :update }
      old_values { { account_name: '現金' } }
      new_values { { account_name: '現金及び預金' } }
      change_data { nil }
    end

    trait :delete do
      action { :delete }
      old_values { { account_name: '現金' } }
      reason { '削除理由' }
      change_data { nil }
    end
  end
end
