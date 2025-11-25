package com.example.accounting.infrastructure.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.springframework.amqp.core.*;
import org.springframework.amqp.rabbit.connection.ConnectionFactory;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.amqp.support.converter.Jackson2JsonMessageConverter;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * RabbitMQ 設定
 *
 * イベント駆動アーキテクチャで使用するメッセージブローカーの設定を行います。
 * Topic Exchange を使用してイベントをルーティングします。
 */
@Configuration
public class RabbitMQConfig {

    public static final String EXCHANGE_NAME = "financial-events";
    public static final String ROUTING_KEY_JOURNAL_CREATED = "financial.journalentry.created";
    public static final String ROUTING_KEY_JOURNAL_APPROVED = "financial.journalentry.approved";
    public static final String ROUTING_KEY_JOURNAL_DELETED = "financial.journalentry.deleted";

    /**
     * Topic Exchange を作成
     *
     * Topic Exchange はルーティングキーのパターンマッチングを使用してメッセージをルーティングします。
     * financial.journalentry.* のようなワイルドカードが使用できます。
     */
    @Bean
    public TopicExchange financialEventsExchange() {
        return ExchangeBuilder
            .topicExchange(EXCHANGE_NAME)
            .durable(true)
            .build();
    }

    /**
     * JSON メッセージコンバーター
     *
     * Java オブジェクトを JSON にシリアライズ・デシリアライズします。
     * Jackson を使用して LocalDateTime などの Java 8 日付型もサポートします。
     */
    @Bean
    public Jackson2JsonMessageConverter jsonMessageConverter() {
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        return new Jackson2JsonMessageConverter(objectMapper);
    }

    /**
     * RabbitTemplate（メッセージ送信用）
     *
     * Spring AMQP のメッセージ送信テンプレート。
     * JSON コンバーターとデフォルト Exchange を設定します。
     */
    @Bean
    public RabbitTemplate rabbitTemplate(
        ConnectionFactory connectionFactory,
        Jackson2JsonMessageConverter jsonMessageConverter
    ) {
        RabbitTemplate template = new RabbitTemplate(connectionFactory);
        template.setMessageConverter(jsonMessageConverter);
        template.setExchange(EXCHANGE_NAME);
        return template;
    }
}
