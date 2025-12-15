# frozen_string_literal: true

module AuditLogRepositoryInterface
  def save(audit_log)
    raise NotImplementedError, "#{self.class}#save must be implemented"
  end

  def find_by_entity(entity_type, entity_id)
    raise NotImplementedError, "#{self.class}#find_by_entity must be implemented"
  end

  def find_by_user(user_id, start_date, end_date)
    raise NotImplementedError, "#{self.class}#find_by_user must be implemented"
  end

  def find_by_period(start_date, end_date, limit)
    raise NotImplementedError, "#{self.class}#find_by_period must be implemented"
  end
end
