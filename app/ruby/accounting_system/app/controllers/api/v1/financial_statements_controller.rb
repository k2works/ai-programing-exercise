# frozen_string_literal: true

# インフラストラクチャ層：Input Adapter
# 財務諸表 REST API コントローラー
module Api
  module V1
    class FinancialStatementsController < ApplicationController
      # GET /api/v1/financial-statements/balance-sheet
      # 貸借対照表を取得
      def balance_sheet
        as_of_date = parse_date(params[:as_of_date])
        balance_sheet = FinancialStatementService.generate_balance_sheet(as_of_date)
        render json: BalanceSheetSerializer.new(balance_sheet).as_json
      rescue ArgumentError => e
        render json: { error: e.message }, status: :bad_request
      end

      # GET /api/v1/financial-statements/income-statement
      # 損益計算書を取得
      def income_statement
        from_date = parse_date(params[:from_date])
        to_date = parse_date(params[:to_date])

        if from_date > to_date
          return render json: { error: '開始日は終了日以前である必要があります' }, status: :bad_request
        end

        income_statement = FinancialStatementService.generate_income_statement(from_date, to_date)
        render json: IncomeStatementSerializer.new(income_statement).as_json
      rescue ArgumentError => e
        render json: { error: e.message }, status: :bad_request
      end

      # GET /api/v1/financial-statements/ratios
      # 財務指標を取得
      def ratios
        as_of_date = parse_date(params[:as_of_date])
        from_date = parse_date(params[:from_date])
        to_date = parse_date(params[:to_date])

        if from_date > to_date
          return render json: { error: '開始日は終了日以前である必要があります' }, status: :bad_request
        end

        balance_sheet = FinancialStatementService.generate_balance_sheet(as_of_date)
        income_statement = FinancialStatementService.generate_income_statement(from_date, to_date)
        financial_ratios = FinancialStatementService.calculate_financial_ratios(balance_sheet, income_statement)

        render json: FinancialRatiosSerializer.new(financial_ratios).as_json
      rescue ArgumentError => e
        render json: { error: e.message }, status: :bad_request
      end

      private

      def parse_date(date_string)
        raise ArgumentError, '日付パラメータが必要です' if date_string.blank?

        Date.parse(date_string)
      rescue Date::Error
        raise ArgumentError, '無効な日付形式です。YYYY-MM-DD形式で指定してください'
      end
    end
  end
end
