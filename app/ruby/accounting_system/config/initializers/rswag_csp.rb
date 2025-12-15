# frozen_string_literal: true

# Monkey-patch Rswag::Ui::Middleware to add connect-src to CSP
# This allows Swagger UI to make fetch requests to the API endpoints
module Rswag
  module Ui
    class Middleware
      private

      def csp
        base_policy = <<~POLICY.tr "\n", ' '
          default-src 'self';
          img-src 'self' data: https://validator.swagger.io;
          font-src 'self' https://fonts.gstatic.com;
          style-src 'self' 'unsafe-inline' https://fonts.googleapis.com;
          script-src 'self' 'unsafe-inline';
        POLICY

        # Add connect-src directive based on environment
        connect_src = if Rails.env.development?
                        "connect-src 'self' http://localhost:3000;"
                      else
                        "connect-src 'self' https:;"
                      end

        # Insert connect-src after default-src
        base_policy.sub('default-src \'self\';', "default-src 'self'; #{connect_src}")
      end
    end
  end
end
