// tests/setup/test-containers.ts

import { PostgreSqlContainer, StartedPostgreSqlContainer } from '@testcontainers/postgresql'
import { RabbitMQContainer, StartedRabbitMQContainer } from '@testcontainers/rabbitmq'
import { execSync } from 'child_process'

/**
 * TestContainers セットアップ
 *
 * 統合テスト用のPostgreSQLとRabbitMQコンテナを管理
 */
export class TestContainersSetup {
  private postgresContainer?: StartedPostgreSqlContainer
  private rabbitmqContainer?: StartedRabbitMQContainer

  /**
   * コンテナを起動
   */
  async start(): Promise<void> {
    console.log('🚀 Starting test containers...')

    // PostgreSQL コンテナの起動
    this.postgresContainer = await new PostgreSqlContainer('postgres:16-alpine')
      .withDatabase('test_management_accounting')
      .withUsername('test_user')
      .withPassword('test_password')
      .withExposedPorts(5432)
      .start()

    console.log('✅ PostgreSQL container started')

    // RabbitMQ コンテナの起動
    this.rabbitmqContainer = await new RabbitMQContainer('rabbitmq:3-management-alpine')
      .withExposedPorts(5672, 15672)
      .start()

    console.log('✅ RabbitMQ container started')

    // 環境変数を設定
    process.env.MANAGEMENT_ACCOUNTING_DATABASE_URL = this.postgresContainer.getConnectionUri()
    process.env.RABBITMQ_URL = this.rabbitmqContainer.getAmqpUrl()

    // Prisma マイグレーションを実行
    await this.runMigrations()

    console.log('✅ Test containers setup complete')
  }

  /**
   * Prisma マイグレーションを実行
   */
  private async runMigrations(): Promise<void> {
    try {
      console.log('🔄 Running Prisma migrations...')
      execSync('npx prisma migrate deploy', {
        stdio: 'inherit',
        env: {
          ...process.env,
          DATABASE_URL: this.postgresContainer!.getConnectionUri()
        }
      })
      console.log('✅ Prisma migrations completed')
    } catch (error) {
      console.error('❌ Failed to run migrations:', error)
      throw error
    }
  }

  /**
   * コンテナを停止
   */
  async stop(): Promise<void> {
    console.log('🛑 Stopping test containers...')

    if (this.postgresContainer) {
      await this.postgresContainer.stop()
      console.log('✅ PostgreSQL container stopped')
    }

    if (this.rabbitmqContainer) {
      await this.rabbitmqContainer.stop()
      console.log('✅ RabbitMQ container stopped')
    }
  }

  /**
   * 接続情報を取得
   */
  getConnectionInfo(): {
    databaseUrl: string
    rabbitmqUrl: string
    postgresHost: string
    postgresPort: number
    rabbitmqHost: string
    rabbitmqPort: number
  } {
    if (!this.postgresContainer || !this.rabbitmqContainer) {
      throw new Error('Containers not started. Call start() first.')
    }

    return {
      databaseUrl: this.postgresContainer.getConnectionUri(),
      rabbitmqUrl: this.rabbitmqContainer.getAmqpUrl(),
      postgresHost: this.postgresContainer.getHost(),
      postgresPort: this.postgresContainer.getPort(),
      rabbitmqHost: this.rabbitmqContainer.getHost(),
      rabbitmqPort: this.rabbitmqContainer.getMappedPort(5672)
    }
  }

  /**
   * データベースをクリーンアップ
   */
  async cleanDatabase(): Promise<void> {
    if (!this.postgresContainer) {
      return
    }

    try {
      execSync('npx prisma migrate reset --force --skip-seed', {
        stdio: 'inherit',
        env: {
          ...process.env,
          DATABASE_URL: this.postgresContainer.getConnectionUri()
        }
      })
    } catch (error) {
      console.error('❌ Failed to clean database:', error)
      throw error
    }
  }
}
