# frozen_string_literal: true

require 'rails_helper'

RSpec.configure do |config|
  config.openapi_root = Rails.root.join('swagger').to_s

  config.openapi_specs = {
    'v1/swagger.yaml' => {
      openapi: '3.0.1',
      info: {
        title: '販売管理システム API V1',
        version: 'v1',
        description: 'TDDで育てる販売管理システムのREST API',
        contact: {
          name: 'API Support',
          email: 'support@example.com'
        },
        license: {
          name: 'MIT',
          url: 'https://opensource.org/licenses/MIT'
        }
      },
      paths: {},
      servers: [
        {
          url: 'http://localhost:3000',
          description: 'Development server'
        },
        {
          url: 'https://api.example.com',
          description: 'Production server'
        }
      ],
      components: {
        schemas: {
          Order: {
            type: 'object',
            properties: {
              id: { type: 'integer', example: 1 },
              order_number: { type: 'string', example: 'SO202501060001' },
              order_type: { type: 'string', enum: ['Sales', 'Purchase'], example: 'Sales' },
              order_date: { type: 'string', format: 'date', example: '2025-01-06' },
              status: { type: 'string', enum: %w[draft confirmed completed cancelled], example: 'draft' },
              party: {
                type: 'object',
                properties: {
                  id: { type: 'integer', example: 1 },
                  name: { type: 'string', example: '株式会社サンプル' }
                }
              },
              items: {
                type: 'array',
                items: { '$ref' => '#/components/schemas/OrderItem' }
              },
              total_amount: { type: 'number', format: 'decimal', example: 15_000 },
              created_at: { type: 'string', format: 'date-time' },
              updated_at: { type: 'string', format: 'date-time' }
            },
            required: %w[id order_number order_type order_date status]
          },
          OrderItem: {
            type: 'object',
            properties: {
              id: { type: 'integer', example: 1 },
              product_id: { type: 'integer', example: 1 },
              product_name: { type: 'string', example: '商品A' },
              quantity: { type: 'integer', example: 10 },
              unit_price: { type: 'number', format: 'decimal', example: 1000 },
              subtotal: { type: 'number', format: 'decimal', example: 10_000 }
            }
          },
          OrderInput: {
            type: 'object',
            properties: {
              order_type: { type: 'string', enum: ['Sales', 'Purchase'], example: 'Sales' },
              order_date: { type: 'string', format: 'date', example: '2025-01-06' },
              party_id: { type: 'integer', example: 1 },
              order_items_attributes: {
                type: 'array',
                items: {
                  type: 'object',
                  properties: {
                    product_id: { type: 'integer', example: 1 },
                    quantity: { type: 'integer', example: 10 },
                    unit_price: { type: 'number', format: 'decimal', example: 1000 }
                  },
                  required: %w[product_id quantity unit_price]
                }
              }
            },
            required: %w[order_type party_id order_items_attributes]
          },
          Error: {
            type: 'object',
            properties: {
              errors: {
                type: 'array',
                items: { type: 'string' }
              }
            }
          },
          PaginationMeta: {
            type: 'object',
            properties: {
              current_page: { type: 'integer', example: 1 },
              total_pages: { type: 'integer', example: 10 },
              total_count: { type: 'integer', example: 100 }
            }
          }
        }
      }
    }
  }

  config.openapi_format = :yaml
end
