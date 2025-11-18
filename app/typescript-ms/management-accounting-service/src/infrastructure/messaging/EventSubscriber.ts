// src/infrastructure/messaging/EventSubscriber.ts

import { connect, ChannelModel, Channel } from 'amqplib'

/**
 * イベントハンドラーの型定義
 */
export type EventHandler<T = any> = (event: T) => Promise<void>

/**
 * RabbitMQ ベースのイベント購読者
 *
 * 他のサービスから発行されたドメインイベントを購読して処理する責務を持つ
 */
export class EventSubscriber {
  private connection: ChannelModel | null = null
  private channel: Channel | null = null
  private readonly exchangeName: string
  private readonly queueName: string
  private handlers: Map<string, EventHandler[]> = new Map()

  constructor(exchangeName: string, queueName: string) {
    this.exchangeName = exchangeName
    this.queueName = queueName
  }

  /**
   * RabbitMQ に接続する
   */
  async connect(): Promise<void> {
    try {
      const rabbitmqUrl = process.env.RABBITMQ_URL || 'amqp://admin:admin@localhost:5672'
      this.connection = await connect(rabbitmqUrl)
      this.channel = await this.connection.createChannel()

      // キューを宣言（存在しない場合は作成）
      await this.channel.assertQueue(this.queueName, {
        durable: true
      })

      console.log(`✅ EventSubscriber connected to RabbitMQ: ${this.queueName}`)
    } catch (error) {
      console.error('❌ Failed to connect to RabbitMQ:', error)
      throw error
    }
  }

  /**
   * イベントハンドラーを登録する
   *
   * @param routingKey ルーティングキー（例: "journal.created", "account.updated"）
   * @param handler イベント処理関数
   */
  on<T = any>(routingKey: string, handler: EventHandler<T>): void {
    if (!this.handlers.has(routingKey)) {
      this.handlers.set(routingKey, [])
    }
    this.handlers.get(routingKey)!.push(handler as EventHandler)
    console.log(`📝 Registered handler for: ${routingKey}`)
  }

  /**
   * イベントの購読を開始する
   */
  async subscribe(): Promise<void> {
    if (!this.channel) {
      throw new Error('EventSubscriber is not connected. Call connect() first.')
    }

    try {
      // 登録されたすべてのルーティングキーに対してバインド
      for (const routingKey of this.handlers.keys()) {
        await this.channel.bindQueue(this.queueName, this.exchangeName, routingKey)
        console.log(`🔗 Bound queue to exchange: ${this.exchangeName} with routing key: ${routingKey}`)
      }

      // メッセージの消費を開始
      await this.channel.consume(
        this.queueName,
        async (msg) => {
          if (!msg) return

          try {
            const routingKey = msg.fields.routingKey
            const content = msg.content.toString()
            const event = JSON.parse(content)

            console.log(`📥 Event received: ${routingKey}`, event)

            // 該当するハンドラーを実行
            const handlers = this.handlers.get(routingKey) || []
            for (const handler of handlers) {
              await handler(event)
            }

            // メッセージを確認（ACK）
            this.channel?.ack(msg)
          } catch (error) {
            console.error('❌ Error processing event:', error)
            // エラーが発生した場合はメッセージを拒否してリキューしない
            this.channel?.nack(msg, false, false)
          }
        },
        {
          noAck: false // 手動でACKする
        }
      )

      console.log(`🎧 Started consuming events from: ${this.queueName}`)
    } catch (error) {
      console.error('❌ Failed to subscribe to events:', error)
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
      console.log('✅ EventSubscriber disconnected from RabbitMQ')
    } catch (error) {
      console.error('❌ Error closing RabbitMQ connection:', error)
    }
  }
}

/**
 * EventSubscriber のシングルトンインスタンス
 */
let eventSubscriber: EventSubscriber | null = null

export function getEventSubscriber(): EventSubscriber {
  if (!eventSubscriber) {
    eventSubscriber = new EventSubscriber(
      'financial-accounting-events', // 財務会計サービスのイベントを購読
      'management-accounting-queue' // 管理会計サービス専用のキュー
    )
  }
  return eventSubscriber
}

export async function closeEventSubscriber(): Promise<void> {
  if (eventSubscriber) {
    await eventSubscriber.close()
    eventSubscriber = null
  }
}
