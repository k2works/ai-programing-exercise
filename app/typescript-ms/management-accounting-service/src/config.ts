// src/config.ts

export const config = {
  // サーバー設定
  port: parseInt(process.env.PORT || '3002', 10),
  host: process.env.HOST || '0.0.0.0',

  // データベース設定
  databaseUrl: process.env.MANAGEMENT_ACCOUNTING_DATABASE_URL ||
    'postgresql://ma_user:ma_password@localhost:5433/management_accounting',

  // 財務会計サービスURL
  financialAccountingServiceUrl: process.env.FINANCIAL_ACCOUNTING_SERVICE_URL ||
    'http://localhost:3001',

  // RabbitMQ 設定
  rabbitmqUrl: process.env.RABBITMQ_URL || 'amqp://admin:admin@localhost:5672',

  // CORS 設定
  corsOrigin: process.env.CORS_ORIGIN || '*',

  // 環境
  nodeEnv: process.env.NODE_ENV || 'development',
  isProduction: process.env.NODE_ENV === 'production',
  isDevelopment: process.env.NODE_ENV !== 'production'
}
