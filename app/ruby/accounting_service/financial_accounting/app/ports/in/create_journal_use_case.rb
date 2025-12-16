# frozen_string_literal: true

module Ports
  module In
    class CreateJournalUseCase
      # ユースケースのインターフェースを定義
      def execute(journal_date:, description:, fiscal_year:, entries:)
        raise NotImplementedError, "#{self.class}##{__method__} must be implemented"
      end
    end
  end
end
