Rails.application.routes.draw do
  # Swagger UI
  mount Rswag::Ui::Engine => '/api-docs'
  mount Rswag::Api::Engine => '/api-docs'

  # API routes
  namespace :api do
    namespace :v1 do
      resources :orders do
        member do
          post :confirm
        end
      end
    end
  end

  # Reveal health status on /up that returns 200 if the app boots with no exceptions, otherwise 500.
  # Can be used by load balancers and uptime monitors to verify that the app is live.
  get "up" => "rails/health#show", as: :rails_health_check

  # Defines the root path route ("/")
  # root "posts#index"
end
