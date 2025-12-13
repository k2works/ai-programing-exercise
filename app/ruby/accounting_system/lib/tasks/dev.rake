# frozen_string_literal: true

namespace :dev do
  desc 'Run all checks (RuboCop, RSpec)'
  task all: :environment do
    puts 'Running RuboCop...'
    system('bundle exec rubocop') || exit(1)

    puts "\nRunning RSpec..."
    system('bundle exec rspec') || exit(1)

    puts "\nAll checks passed!"
  end

  desc 'Run RuboCop with auto-correct'
  task lint: :environment do
    system('bundle exec rubocop -A')
  end

  desc 'Reset database'
  task reset: :environment do
    Rake::Task['db:drop'].invoke
    Rake::Task['db:create'].invoke
    Rake::Task['db:migrate'].invoke
    Rake::Task['db:seed'].invoke if Rails.env.development?
  end
end
