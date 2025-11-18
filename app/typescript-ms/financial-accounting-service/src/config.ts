// src/config.ts

export const config = {
  // サーバー設定
  port: parseInt(process.env.PORT || '3001', 10),
  host: process.env.HOST || '0.0.0.0',

  // データベース設定
  databaseUrl: process.env.FINANCIAL_ACCOUNTING_DATABASE_URL ||
    'postgresql://fa_user:fa_password@localhost:5432/financial_accounting',

  // RabbitMQ 設定
  rabbitmqUrl: process.env.RABBITMQ_URL || 'amqp://admin:admin@localhost:5672',

  // CORS 設定
  corsOrigin: process.env.CORS_ORIGIN || '*',

  // 環境
  nodeEnv: process.env.NODE_ENV || 'development',
  isProduction: process.env.NODE_ENV === 'production',
  isDevelopment: process.env.NODE_ENV !== 'production'
}
