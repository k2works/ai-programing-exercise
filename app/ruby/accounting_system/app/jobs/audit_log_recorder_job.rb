# frozen_string_literal: true

class AuditLogRecorderJob < ApplicationJob
  queue_as :default

  def perform(entity_type, entity_id, action, user_id, user_name, old_values, new_values, change_data, ip_address)
    case action
    when 'create'
      AuditLog.create_log(
        entity_type: entity_type,
        entity_id: entity_id,
        action: :create,
        user_id: user_id,
        user_name: user_name,
        change_data: change_data,
        ip_address: ip_address
      )
    when 'update'
      AuditLog.create_for_update(
        entity_type: entity_type,
        entity_id: entity_id,
        user_id: user_id,
        user_name: user_name,
        old_values: old_values,
        new_values: new_values,
        ip_address: ip_address
      )
    when 'delete'
      AuditLog.create_for_delete(
        entity_type: entity_type,
        entity_id: entity_id,
        user_id: user_id,
        user_name: user_name,
        old_values: old_values,
        reason: change_data&.dig('reason'),
        ip_address: ip_address
      )
    end
  end
end
