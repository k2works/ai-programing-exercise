Rails.application.routes.draw do
  mount Rswag::Ui::Engine => '/api-docs'
  mount Rswag::Api::Engine => '/api-docs'
  # Define your application routes per the DSL in https://guides.rubyonrails.org/routing.html

  # Reveal health status on /up that returns 200 if the app boots with no exceptions, otherwise 500.
  # Can be used by load balancers and uptime monitors to verify that the app is live.
  get "up" => "rails/health#show", as: :rails_health_check

  # API routes
  namespace :api do
    namespace :v1 do
      resources :accounts, param: :code
      resources :journal_entries, path: 'journal-entries'

      # 財務諸表エンドポイント
      get 'financial-statements/balance-sheet', to: 'financial_statements#balance_sheet'
      get 'financial-statements/income-statement', to: 'financial_statements#income_statement'
      get 'financial-statements/ratios', to: 'financial_statements#ratios'

      # 監査ログAPI
      get 'audit_logs/entity/:entity_type/:entity_id', to: 'audit_logs#entity_history'
      get 'audit_logs/user/:user_id', to: 'audit_logs#user_activity'
      get 'audit_logs/period', to: 'audit_logs#period_logs'
    end
  end

  # Defines the root path route ("/")
  # Redirect root to Swagger UI
  root to: redirect('/api-docs')
end
