// src/infrastructure/messaging/EventPublisher.ts

import { connect, ChannelModel, Channel } from 'amqplib'
import { config } from '../../config'

/**
 * RabbitMQ ベースのイベント発行者
 *
 * ドメインイベントをRabbitMQに発行する責務を持つ
 */
export class EventPublisher {
  private connection: ChannelModel | null = null
  private channel: Channel | null = null
  private readonly exchangeName = 'financial-accounting-events'
  private readonly exchangeType = 'topic'

  /**
   * RabbitMQ に接続する
   */
  async connect(): Promise<void> {
    try {
      const rabbitmqUrl = process.env.RABBITMQ_URL || 'amqp://admin:admin@localhost:5672'
      this.connection = await connect(rabbitmqUrl)
      this.channel = await this.connection.createChannel()

      // Exchange を宣言（存在しない場合は作成）
      await this.channel.assertExchange(this.exchangeName, this.exchangeType, {
        durable: true
      })

      console.log(`✅ EventPublisher connected to RabbitMQ: ${this.exchangeName}`)
    } catch (error) {
      console.error('❌ Failed to connect to RabbitMQ:', error)
      throw error
    }
  }

  /**
   * イベントを発行する
   *
   * @param routingKey ルーティングキー（例: "journal.created", "account.updated"）
   * @param event イベントデータ
   */
  async publish<T>(routingKey: string, event: T): Promise<void> {
    if (!this.channel) {
      throw new Error('EventPublisher is not connected. Call connect() first.')
    }

    try {
      const message = JSON.stringify(event)
      const buffer = Buffer.from(message)

      this.channel.publish(this.exchangeName, routingKey, buffer, {
        persistent: true,
        contentType: 'application/json',
        timestamp: Date.now()
      })

      console.log(`📤 Event published: ${routingKey}`, event)
    } catch (error) {
      console.error(`❌ Failed to publish event: ${routingKey}`, error)
      throw error
    }
  }

  /**
   * 接続を閉じる
   */
  async close(): Promise<void> {
    try {
      if (this.channel) {
        await this.channel.close()
        this.channel = null
      }
      if (this.connection) {
        await this.connection.close()
        this.connection = null
      }
      console.log('✅ EventPublisher disconnected from RabbitMQ')
    } catch (error) {
      console.error('❌ Error closing RabbitMQ connection:', error)
    }
  }
}

/**
 * EventPublisher のシングルトンインスタンス
 */
let eventPublisher: EventPublisher | null = null

export function getEventPublisher(): EventPublisher {
  if (!eventPublisher) {
    eventPublisher = new EventPublisher()
  }
  return eventPublisher
}

export async function closeEventPublisher(): Promise<void> {
  if (eventPublisher) {
    await eventPublisher.close()
    eventPublisher = null
  }
}
